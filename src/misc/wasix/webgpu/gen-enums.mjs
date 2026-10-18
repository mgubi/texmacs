// The enumerated values of webgpu.h as the strings of the WebGPU of
// JavaScript, for the host of webgpu_wire.cpp (webgpu-host.js):
//
//   node misc/wasix/webgpu/gen-enums.mjs <webgpu.h> <webgpu-enums.js>
//
// The names of webgpu.h become those of JavaScript (TriangleList:
// "triangle-list"; the formats have names of their own); Undefined and
// BindingNotUsed have none (the default of JavaScript).

import fs from 'node:fs';

const [header, out] = process.argv.slice (2);
const text = fs.readFileSync (header, 'utf8');
// the kinds, in the order of wire_kind in webgpu_wire.cpp
const KINDS = ['TextureFormat', 'VertexFormat', 'PrimitiveTopology', 'IndexFormat',
  'FrontFace', 'CullMode', 'CompareFunction', 'StencilOperation', 'BlendOperation',
  'BlendFactor', 'LoadOp', 'StoreOp', 'AddressMode', 'FilterMode', 'MipmapFilterMode',
  'TextureDimension', 'TextureViewDimension', 'TextureAspect', 'BufferBindingType',
  'SamplerBindingType', 'TextureSampleType', 'StorageTextureAccess', 'VertexStepMode',
  'CompositeAlphaMode'];

function kebab (n) {
  return n.replace (/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase ();
}

function jsName (kind, n) {
  if (['Undefined', 'BindingNotUsed', 'Force32'].includes (n)) return null;
  if (kind === 'CompositeAlphaMode' && (n === 'Auto' || n === 'Inherit')) return null;
  if (kind === 'TextureFormat') {
    let s = n.toLowerCase ();
    s = s.replace (/srgb$/, '-srgb');
    s = s.replace ('depth24plusstencil8', 'depth24plus-stencil8');
    s = s.replace ('depth32floatstencil8', 'depth32float-stencil8');
    // the compressed formats: bc1-rgba-unorm, etc2-rgb8unorm, astc-4x4-unorm
    s = s.replace (/^bc(\d+)(rgba|rgb|rg|r)(.*)$/, 'bc$1-$2-$3');
    s = s.replace (/^(etc2|eac)(.*?)(unorm|snorm)/, '$1-$2$3');
    s = s.replace (/^astc(\d+x\d+)(.*)$/, 'astc-$1-$2');
    return s;
  }
  if (kind === 'VertexFormat')
    return n.toLowerCase ().replace (/_/g, '-').replace ('x4bgra', 'x4-bgra');
  if (kind === 'TextureDimension' || kind === 'TextureViewDimension')
    return { '1D': '1d', '2D': '2d', '3D': '3d', '2DArray': '2d-array',
             'Cube': 'cube', 'CubeArray': 'cube-array' }[n];
  return kebab (n);
}

const tables = [null];
for (const kind of KINDS) {
  const m = new RegExp (`typedef enum WGPU${kind} \\{([\\s\\S]*?)\\}`).exec (text);
  if (!m) throw new Error (`no enum WGPU${kind} in ${header}`);
  const t = {};
  for (const [, name, value] of m[1].matchAll (new RegExp (`WGPU${kind}_(\\w+) = (0x[0-9A-Fa-f]+)`, 'g'))) {
    const s = jsName (kind, name);
    if (s) t[Number (value)] = s;
  }
  tables.push (t);
}
fs.writeFileSync (out, '// made by misc/wasix/webgpu/gen-enums.mjs from webgpu.h: do not edit\n' +
  `export const KINDS = ${JSON.stringify ([null, ...KINDS])};\n` +
  `export const ENUMS = ${JSON.stringify (tables)};\n`);
console.log (`${out}: ${KINDS.length} kinds`);
