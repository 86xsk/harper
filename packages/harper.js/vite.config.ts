/// <reference types="vitest" />
import { resolve } from 'path';
import dts from 'vite-plugin-dts';
import { defineConfig, type Plugin } from 'vite';

function removeAssetsPlugin(options: { test: RegExp }): Plugin {
	return {
		name: 'remove-wasm',
		generateBundle(_, bundle) {
			for (const file in bundle) {
				if (options.test.test(file)) {
					delete bundle[file];
				}
			}
		}
	};
}

export default defineConfig({
	build: {
		lib: {
			entry: resolve(__dirname, 'src/main.ts'),
			fileName: `harper`,
			name: 'harper',
			formats: ['es']
		},
		minify: false,
		assetsInlineLimit: 0,
		rollupOptions: {
			output: {
				minifyInternalExports: false,
				inlineDynamicImports: true
			},
			treeshake: {
				moduleSideEffects: false,
				propertyReadSideEffects: false
			}
		}
	},
	base: './',
	plugins: [
		dts({
			...require('./api-extractor.json'),
			rollupTypes: true,
			tsconfigPath: './tsconfig.json'
		})
	],
	worker: {
		format: 'es',
		plugins: () => [removeAssetsPlugin({ test: /\.wasm$/ })],
		rollupOptions: {
			output: {
				inlineDynamicImports: true
			}
		}
	},
	server: {
		fs: {
			allow: ['../../harper-wasm/pkg']
		}
	},
	test: {
		browser: {
			provider: 'playwright',
			enabled: true,
			headless: true,
			screenshotFailures: false,
			instances: [{ browser: 'chromium' }, { browser: 'firefox' }]
		}
	},
	assetsInclude: ['**/*.wasm']
});
