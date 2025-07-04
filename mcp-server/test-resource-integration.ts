import { McpServer, ResourceTemplate } from '@modelcontextprotocol/sdk/server/mcp.js';
import { InMemoryTransport } from '@modelcontextprotocol/sdk/inMemory.js';
import { Client } from '@modelcontextprotocol/sdk/client/index.js';

async function testResourceIntegration() {
  console.log('Testing ResourceTemplate integration...\n');
  
  // Create server
  const server = new McpServer({
    name: 'test-server',
    version: '1.0.0'
  });
  
  // Create a ResourceTemplate with list callback
  const template = new ResourceTemplate(
    'test://buffer/{path}',
    {
      list: async () => {
        console.log('ResourceTemplate.list() called');
        return {
          resources: [
            { uri: 'test://buffer/file1.ts', name: 'file1.ts', description: 'First file' },
            { uri: 'test://buffer/file2.ts', name: 'file2.ts', description: 'Second file' }
          ]
        };
      }
    }
  );
  
  // Register the resource with the server
  server.registerResource(
    'test-buffers',
    template,
    {
      title: 'Test Buffers',
      description: 'Test buffer resources',
      mimeType: 'text/plain'
    },
    async (uri, variables) => {
      console.log(`Read callback called with uri: ${uri}, path: ${variables.path}`);
      return {
        contents: [{
          uri: uri.toString(),
          mimeType: 'text/plain',
          text: `Content of ${variables.path}`
        }]
      };
    }
  );
  
  // Create client
  const client = new Client({
    name: 'test-client',
    version: '1.0.0'
  });
  
  // Connect client and server
  const [clientTransport, serverTransport] = InMemoryTransport.createLinkedPair();
  await Promise.all([
    client.connect(clientTransport),
    server.connect(serverTransport)
  ]);
  
  console.log('\n1. Listing resources...');
  const listResult = await client.listResources();
  console.log('Resources:', JSON.stringify(listResult, null, 2));
  
  console.log('\n2. Listing resource templates...');
  const templatesResult = await client.listResourceTemplates();
  console.log('Templates:', JSON.stringify(templatesResult, null, 2));
  
  console.log('\n3. Reading a resource...');
  const readResult = await client.readResource({ uri: 'test://buffer/myfile.ts' });
  console.log('Read result:', JSON.stringify(readResult, null, 2));
  
  // Close connections
  await client.close();
  await server.close();
}

testResourceIntegration().catch(console.error);