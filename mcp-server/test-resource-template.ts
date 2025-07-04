import { ResourceTemplate } from '@modelcontextprotocol/sdk/server/mcp.js';

// Test ResourceTemplate behavior
async function testResourceTemplate() {
  console.log('Testing ResourceTemplate...');
  
  // Create a template with dynamic resources
  const template = new ResourceTemplate(
    'test://resource/{id}',
    {
      list: async () => {
        console.log('list() called');
        const resources = [
          { uri: 'test://resource/1', name: 'Resource 1' },
          { uri: 'test://resource/2', name: 'Resource 2' },
        ];
        console.log('Returning resources:', resources);
        return { resources };
      }
    }
  );
  
  // Check template properties
  console.log('Template uriTemplate:', template.uriTemplate);
  console.log('Template hasListMethod:', 'list' in template);
  
  // Try to call list method
  if ('list' in template) {
    const result = await (template as any).list();
    console.log('Direct list() result:', result);
  }
}

testResourceTemplate().catch(console.error);