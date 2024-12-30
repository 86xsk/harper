import { DeserializedRequest, deserializeArg, serialize } from './communication';
import type { Lint, Suggestion, Span } from 'wasm';
import Linter from '../Linter';
import Worker from './worker.js?worker&inline';
import { getWasmUri } from '../loadWasm';
import { LintConfig } from '../main';

/** The data necessary to complete a request once the worker has responded. */
type RequestItem = {
	resolve: (item: unknown) => void;
	reject: (item: unknown) => void;
	request: DeserializedRequest;
};

/** A Linter that spins up a dedicated web worker to do processing on a separate thread.
 * Main benefit: this Linter will not block the event loop for large documents.
 *
 * NOTE: This class will not work properly in Node. In that case, just use `LocalLinter`.
 * Also requires top-level await to work. */
export default class WorkerLinter implements Linter {
	private worker;
	private requestQueue: RequestItem[];
	private working = true;

	constructor() {
		this.worker = new Worker();
		this.requestQueue = [];

		// Fires when the worker sends 'ready'.
		this.worker.onmessage = () => {
			this.setupMainEventListeners();

			this.worker.postMessage(getWasmUri());

			this.working = false;
			this.submitRemainingRequests();
		};
	}

	private setupMainEventListeners() {
		this.worker.onmessage = (e: MessageEvent) => {
			const { resolve } = this.requestQueue.shift()!;
			deserializeArg(e.data).then((v) => {
				resolve(v);

				this.working = false;

				this.submitRemainingRequests();
			});
		};

		this.worker.onmessageerror = (e: MessageEvent) => {
			const { reject } = this.requestQueue.shift()!;
			reject(e.data);
			this.working = false;

			this.submitRemainingRequests();
		};
	}

	setup(): Promise<void> {
		return this.rpc('setup', []);
	}

	lint(text: string): Promise<Lint[]> {
		return this.rpc('lint', [text]);
	}

	applySuggestion(text: string, suggestion: Suggestion, span: Span): Promise<string> {
		return this.rpc('applySuggestion', [text, suggestion, span]);
	}

	isLikelyEnglish(text: string): Promise<boolean> {
		return this.rpc('isLikelyEnglish', [text]);
	}

	isolateEnglish(text: string): Promise<string> {
		return this.rpc('isolateEnglish', [text]);
	}

	async getLintConfig(): Promise<LintConfig> {
		return JSON.parse(await this.getLintConfigAsJSON());
	}

	setLintConfig(config: LintConfig): Promise<void> {
		return this.setLintConfigWithJSON(JSON.stringify(config));
	}

	getLintConfigAsJSON(): Promise<string> {
		return this.rpc('getLintConfigAsJSON', []);
	}

	setLintConfigWithJSON(config: string): Promise<void> {
		return this.rpc('setLintConfigWithJSON', [config]);
	}

	toTitleCase(text: string): Promise<string> {
		return this.rpc('toTitleCase', [text]);
	}

	/** Run a procedure on the remote worker. */
	private async rpc(procName: string, args: any[]): Promise<any> {
		const promise = new Promise((resolve, reject) => {
			this.requestQueue.push({
				resolve,
				reject,
				request: { procName, args }
			});

			this.submitRemainingRequests();
		});

		return promise;
	}

	private async submitRemainingRequests() {
		if (this.working) {
			return;
		}

		this.working = true;

		if (this.requestQueue.length > 0) {
			const { request } = this.requestQueue[0];

			this.worker.postMessage(await serialize(request));
		} else {
			this.working = false;
		}
	}
}
