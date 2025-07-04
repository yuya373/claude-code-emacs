import { EmacsBridge } from '../emacs-bridge.js';
import { Resource, ResourceContents } from '@modelcontextprotocol/sdk/types.js';

export interface ResourceHandler {
  list(bridge: EmacsBridge): Promise<Resource[]>;
  read(bridge: EmacsBridge, uri: string): Promise<ResourceContents>;
}

export { bufferResourceHandler } from './buffer-resource.js';
export { projectResourceHandler } from './project-resource.js';
