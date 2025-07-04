# ResourceTemplate Usage Guide

## Correct Usage with MCP SDK

When using `ResourceTemplate` with `server.registerResource()`, the correct signature is:

```typescript
server.registerResource(
  name: string,
  template: ResourceTemplate,
  metadata: ResourceMetadata,  // optional
  readCallback: ReadResourceTemplateCallback
)
```

### Key Points

1. The `ResourceTemplate` instance is passed as the second parameter directly
2. The metadata object (title, description, mimeType) is the third parameter
3. The read callback receives three parameters:
   - `uri: URL` - The parsed URL object
   - `variables: Record<string, string | string[]>` - Extracted template variables
   - `extra: RequestHandlerExtra` - Request context

### Example

```typescript
import { ResourceTemplate } from '@modelcontextprotocol/sdk/server/mcp.js';

// Create template with list callback
const bufferTemplate = new ResourceTemplate(
  'emacs://buffer/{path}',
  {
    list: async () => {
      const resources = await getBufferList();
      return { resources };
    }
  }
);

// Register with server
server.registerResource(
  'emacs-buffers',
  bufferTemplate,
  {
    title: 'Emacs Buffers',
    description: 'Open buffers in Emacs',
    mimeType: 'text/plain'
  },
  async (uri, variables, extra) => {
    // uri is a URL object
    // variables.path contains the extracted path from the template
    const content = await readBuffer(variables.path);
    return {
      contents: [{
        uri: uri.toString(),
        mimeType: 'text/plain',
        text: content
      }]
    };
  }
);
```

### Common Mistakes

- ❌ Passing ResourceTemplate as part of config object
- ❌ Using only `uri` parameter in the read callback
- ❌ Not returning `{ resources: [...] }` from list callback

### References

- See `src/server/mcp.ts` in the MCP SDK for type definitions
- The `Variables` type is from `src/shared/uriTemplate.ts`