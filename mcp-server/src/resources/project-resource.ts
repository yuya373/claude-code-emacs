import { Resource, ResourceContents, TextResourceContents } from '@modelcontextprotocol/sdk/types.js';
import { EmacsBridge } from '../emacs-bridge.js';
import { ResourceHandler } from './index.js';
import * as fs from 'fs/promises';
import * as path from 'path';

export const projectResourceHandler: ResourceHandler = {
  async list(bridge: EmacsBridge): Promise<Resource[]> {
    return [
      {
        uri: 'emacs://project/info',
        name: 'Project Information',
        description: 'Current project metadata and status',
        mimeType: 'application/json'
      },
      {
        uri: 'emacs://project/files',
        name: 'Project Files',
        description: 'List of all files in the current project',
        mimeType: 'application/json'
      }
    ];
  },

  async read(bridge: EmacsBridge, uri: string): Promise<ResourceContents> {
    switch (uri) {
      case 'emacs://project/info':
        return await getProjectInfo(bridge);
      case 'emacs://project/files':
        return await getProjectFiles(bridge);
      default:
        throw new Error(`Unknown project resource: ${uri}`);
    }
  }
};

async function getProjectInfo(bridge: EmacsBridge): Promise<TextResourceContents> {
  try {
    const result = await bridge.sendRequest('get-project-info', {});

    const projectInfo = {
      root: result.projectRoot || process.cwd(),
      name: result.projectName || path.basename(process.cwd()),
      type: result.projectType || 'unknown',
      vcs: result.vcs || null,
      branch: result.branch || null,
      lastModified: result.lastModified || new Date().toISOString()
    };

    return {
      uri: 'emacs://project/info',
      mimeType: 'application/json',
      text: JSON.stringify(projectInfo, null, 2)
    };
  } catch (error) {
    // Fallback to basic info if Emacs request fails
    const projectRoot = process.cwd();
    const projectInfo = {
      root: projectRoot,
      name: path.basename(projectRoot),
      type: 'unknown',
      vcs: null,
      branch: null,
      lastModified: new Date().toISOString()
    };

    return {
      uri: 'emacs://project/info',
      mimeType: 'application/json',
      text: JSON.stringify(projectInfo, null, 2)
    };
  }
}

async function getProjectFiles(bridge: EmacsBridge): Promise<TextResourceContents> {
  try {
    const result = await bridge.sendRequest('get-project-files', {});

    if (!result.success || !Array.isArray(result.files)) {
      throw new Error('Failed to get project files from Emacs');
    }

    return {
      uri: 'emacs://project/files',
      mimeType: 'application/json',
      text: JSON.stringify(result.files, null, 2)
    };
  } catch (error) {
    // Fallback: return empty list
    return {
      uri: 'emacs://project/files',
      mimeType: 'application/json',
      text: JSON.stringify([], null, 2)
    };
  }
}
