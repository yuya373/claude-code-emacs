import { WebSocketServer, WebSocket } from 'ws';
import { EventEmitter } from 'events';

interface JsonRpcRequest {
  jsonrpc: '2.0';
  id: number | string;
  method: string;
  params?: any;
}

interface JsonRpcResponse {
  jsonrpc: '2.0';
  id: number | string;
  result?: any;
  error?: {
    code: number;
    message: string;
    data?: any;
  };
}

export class EmacsBridge extends EventEmitter {
  private wss?: WebSocketServer;
  private clients: Map<string, WebSocket> = new Map();
  private sessionId?: string;
  private pendingRequests: Map<number | string, {
    resolve: (result: any) => void;
    reject: (error: any) => void;
  }> = new Map();
  private requestId = 0;
  private log: (message: string) => void;

  constructor(logger?: (message: string) => void) {
    super();
    this.log = logger || (() => {});
  }

  async start(port: number = 0, sessionId?: string): Promise<number> {
    this.sessionId = sessionId;
    return new Promise((resolve, reject) => {
      try {
        this.wss = new WebSocketServer({
          port,
          verifyClient: (info, cb) => {
            try {
              this.log(`WebSocket upgrade request - origin: ${info.origin}, url: ${info.req.url}, headers: ${JSON.stringify(info.req.headers)}`);
              // Accept all connections for now
              cb(true);
            } catch (error) {
              this.log(`Error in verifyClient: ${error}`);
              cb(false, 400, 'Bad Request');
            }
          }
        });

        this.wss.on('connection', (ws, req) => {
          this.log(`WebSocket connection attempt - URL: ${req.url}, headers: ${JSON.stringify(req.headers)}`);

          try {
            const url = new URL(req.url || '', `http://${req.headers.host}`);
            const clientSessionId = decodeURIComponent(url.searchParams.get('session') || 'default');

            this.log(`Emacs connected for session: ${clientSessionId}`);
            this.clients.set(clientSessionId, ws);

            ws.on('message', (data) => {
              try {
                const message = JSON.parse(data.toString());
                this.handleMessage(ws, message);
              } catch (error) {
                this.log(`Invalid message: ${error}`);
              }
            });

            ws.on('close', () => {
              this.log(`Emacs disconnected for session: ${clientSessionId}`);
              this.clients.delete(clientSessionId);
            });

            ws.on('error', (error) => {
              this.log(`WebSocket error: ${error}`);
            });
          } catch (error) {
            this.log(`Error handling WebSocket connection: ${error}`);
            ws.close(1002, 'Invalid request');
          }
        });

      this.wss.on('listening', () => {
        const assignedPort = (this.wss!.address() as any).port;
        this.log(`Emacs bridge listening on port ${assignedPort}`);
        resolve(assignedPort);
      });

      this.wss.on('error', (error) => {
        this.log(`WebSocketServer error: ${error}`);
        reject(error);
      });

      // Add additional error handling
      this.wss.on('headers', (headers, req) => {
        this.log(`WebSocket headers event - URL: ${req.url}`);
      });
      } catch (error) {
        this.log(`Failed to create WebSocketServer: ${error}`);
        reject(error);
      }
    });
  }

  async stop(): Promise<void> {
    if (this.wss) {
      this.clients.forEach((client) => client.close());
      this.clients.clear();

      return new Promise((resolve) => {
        this.wss!.close(() => resolve());
      });
    }
  }

  private handleMessage(ws: WebSocket, message: any): void {
    // Handle ping message
    if ('type' in message && message.type === 'ping') {
      // Respond with pong
      ws.send(JSON.stringify({ type: 'pong' }));
      return;
    }

    // Handle JSON-RPC response
    if ('id' in message && ('result' in message || 'error' in message)) {
      const pending = this.pendingRequests.get(message.id);
      if (pending) {
        this.pendingRequests.delete(message.id);
        if ('error' in message) {
          this.log(`Emacs Response Error: id=${message.id}, error=${JSON.stringify(message.error)}`);
          pending.reject(new Error(message.error.message));
        } else {
          this.log(`Emacs Response: id=${message.id}, result=${JSON.stringify(message.result)}`);
          pending.resolve(message.result);
        }
      }
    }
    // Handle JSON-RPC request from Emacs (if needed)
    else if ('method' in message) {
      // Currently we don't expect requests from Emacs
      this.sendResponse(ws, message.id, null, {
        code: -32601,
        message: 'Method not found'
      });
    }
  }

  private sendResponse(ws: WebSocket, id: number | string, result?: any, error?: any): void {
    const response: JsonRpcResponse = {
      jsonrpc: '2.0',
      id
    };

    if (error) {
      response.error = error;
    } else {
      response.result = result;
    }

    ws.send(JSON.stringify(response) + '\n');
  }

  async request(method: string, params?: any): Promise<any> {
    const sessionClient = this.sessionId ? this.clients.get(this.sessionId) : null;
    const client = sessionClient || Array.from(this.clients.values())[0];

    if (!client) {
      this.log(`Request failed: No Emacs client connected for session ${this.sessionId}`);
      throw new Error('No Emacs client connected');
    }
    const id = ++this.requestId;

    const request: JsonRpcRequest = {
      jsonrpc: '2.0',
      id,
      method,
      params
    };

    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });

      this.log(`Emacs Request: ${method} with params: ${JSON.stringify(params)}`);

      client.send(JSON.stringify(request) + '\n', (error) => {
        if (error) {
          this.pendingRequests.delete(id);
          this.log(`Emacs Request Error: ${method} - ${error}`);
          reject(error);
        }
      });

      // Timeout after 30 seconds
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          this.log(`Emacs Request Timeout: ${method} (id=${id}) after 30 seconds`);
          reject(new Error(`Request timeout: ${method}`));
        }
      }, 30000);
    });
  }

  isConnected(): boolean {
    return this.clients.size > 0;
  }

  async sendRequest(method: string, params?: any): Promise<any> {
    return this.request(method, params);
  }
}
