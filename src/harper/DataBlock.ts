import { getNodesFromQuerySelector, getRichTextContainers } from './domUtils';
import RichText from './RichText';
import { dispatch } from '@wordpress/data';

/**
 * Represents a Gutenberg block on-screen.
 * So named because all of these blocks have a `data-block` attribute.
 */
export default class DataBlock {
	public readonly targetElement: Element;

	constructor(targetElement: Element) {
		this.targetElement = targetElement;
	}

	private getClientId(): string {
		return this.targetElement.getAttribute('data-block')!;
	}

	public getAllRichText(): RichText[] {
		const containers = getRichTextContainers(this.targetElement);

		return containers.map(
			(cont) =>
				new RichText(cont, this, async (newContent: string) => {
					const { updateBlockAttributes } =
						dispatch('core/block-editor');

					const attributeName =
						cont.getAttribute('data-wp-block-attribute-key') ??
						'content';

					await updateBlockAttributes(this.getClientId(), {
						[attributeName]: newContent,
					});
				})
		);
	}

	public static getAllDataBlocks(): DataBlock[] {
		const container = this.getContainer();

		const targetNodes = [
			...getNodesFromQuerySelector(container, '[data-block]'),
		];

		return targetNodes.map((node) => new DataBlock(node));
	}

	public static getContainer(): Element {
		const iframe = document.querySelector('iframe[name="editor-canvas"]');
		const iframeDocument =
			iframe?.contentDocument || iframe?.contentWindow.document;
		const container =
			iframeDocument?.body ||
			document.querySelector('.edit-post-visual-editor > div');
		return container;
	}
}
