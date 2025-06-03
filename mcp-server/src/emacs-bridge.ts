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
  private clients: Set<WebSocket> = new Set();
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

  async start(port: number): Promise<void> {
    return new Promise((resolve, reject) => {
      this.wss = new WebSocketServer({ port });

      this.wss.on('connection', (ws) => {
        this.log('Emacs connected');
        this.clients.add(ws);

        ws.on('message', (data) => {
          try {
            const message = JSON.parse(data.toString());
            this.handleMessage(ws, message);
          } catch (error) {
            this.log(`Invalid message: ${error}`);
          }
        });

        ws.on('close', () => {
          this.log('Emacs disconnected');
          this.clients.delete(ws);
        });

        ws.on('error', (error) => {
          this.log(`WebSocket error: ${error}`);
        });
      });

      this.wss.on('listening', () => {
        this.log(`Emacs bridge listening on port ${port}`);
        resolve();
      });

      this.wss.on('error', (error) => {
        reject(error);
      });
    });
  }

  async stop(): Promise<void> {
    if (this.wss) {
      this.clients.forEach(client => client.close());
      this.clients.clear();

      return new Promise((resolve) => {
        this.wss!.close(() => resolve());
      });
    }
  }

  private handleMessage(ws: WebSocket, message: any): void {
    // Handle JSON-RPC response
    if ('id' in message && ('result' in message || 'error' in message)) {
      const pending = this.pendingRequests.get(message.id);
      if (pending) {
        this.pendingRequests.delete(message.id);
        if ('error' in message) {
          pending.reject(new Error(message.error.message));
        } else {
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
    if (this.clients.size === 0) {
      this.log('Request failed: No Emacs client connected');
      throw new Error('No Emacs client connected');
    }

    // Use first connected client
    const client = Array.from(this.clients)[0];
    const id = ++this.requestId;

    const request: JsonRpcRequest = {
      jsonrpc: '2.0',
      id,
      method,
      params
    };

    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });

      client.send(JSON.stringify(request) + '\n', (error) => {
        if (error) {
          this.pendingRequests.delete(id);
          reject(error);
        }
      });

      // Timeout after 30 seconds
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error(`Request timeout: ${method}`));
        }
      }, 30000);
    });
  }

  isConnected(): boolean {
    return this.clients.size > 0;
  }
}
