import { binary, loadBinary } from '../binary';

/** This module aims to define the communication protocol between the main thread and the worker.
 * Note that much of the complication here comes from the fact that we can't serialize function calls or referenced WebAssembly memory.*/

export type Type =
	| 'string'
	| 'number'
	| 'boolean'
	| 'Suggestion'
	| 'Lint'
	| 'Span'
	| 'Array'
	| 'undefined';

/** Serializable argument to a procedure to be run on the web worker. */
export type RequestArg = {
	json: string;
	type: Type;
};

export async function serialize(req: DeserializedRequest): Promise<SerializedRequest> {
	return {
		procName: req.procName,
		args: await Promise.all(req.args.map(serializeArg))
	};
}

export async function serializeArg(arg: any): Promise<RequestArg> {
	const { Lint, Span, Suggestion } = await loadBinary(binary);

	if (Array.isArray(arg)) {
		return { json: JSON.stringify(await Promise.all(arg.map(serializeArg))), type: 'Array' };
	}

	switch (typeof arg) {
		case 'string':
		case 'number':
		case 'boolean':
		case 'undefined':
			// @ts-expect-error see the `Type` type.
			return { json: JSON.stringify(arg), type: typeof arg };
	}

	if (arg.to_json != undefined) {
		const json = arg.to_json();
		let type: Type | undefined = undefined;

		if (arg instanceof Lint) {
			type = 'Lint';
		} else if (arg instanceof Suggestion) {
			type = 'Suggestion';
		} else if (arg instanceof Span) {
			type = 'Span';
		}

		if (type == undefined) {
			throw new Error('Unhandled case');
		}

		return { json, type };
	}

	throw new Error('Unhandled case');
}

export async function deserializeArg(requestArg: RequestArg): Promise<any> {
	const { Lint, Span, Suggestion } = await loadBinary(binary);

	switch (requestArg.type) {
		case 'undefined':
			return undefined;
		case 'boolean':
		case 'number':
		case 'string':
			return JSON.parse(requestArg.json);
		case 'Suggestion':
			return Suggestion.from_json(requestArg.json);
		case 'Lint':
			return Lint.from_json(requestArg.json);
		case 'Span':
			return Span.from_json(requestArg.json);
		case 'Array':
			return await Promise.all(JSON.parse(requestArg.json).map(deserializeArg));
		default:
			throw new Error(`Unhandled case: ${requestArg.type}`);
	}
}

/** An object that is sent to the web worker to request work to be done. */
export type SerializedRequest = {
	/** The procedure to be executed. */
	procName: string;
	/** The arguments to the procedure */
	args: RequestArg[];
};

export function isSerializedRequest(v: unknown): v is SerializedRequest {
	return typeof v === 'object' && v !== null && 'procName' in v && 'args' in v;
}

/** An object that is received by the web worker to request work to be done. */
export type DeserializedRequest = {
	/** The procedure to be executed. */
	procName: string;
	/** The arguments to the procedure */
	args: any[];
};

export async function deserialize(request: SerializedRequest): Promise<DeserializedRequest> {
	return {
		procName: request.procName,
		args: await Promise.all(request.args.map(deserializeArg))
	};
}
