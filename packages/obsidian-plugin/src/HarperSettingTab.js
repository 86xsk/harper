import './index.js';
import { startCase } from 'lodash-es';
import { PluginSettingTab, Setting } from 'obsidian';

export class HarperSettingTab extends PluginSettingTab {
	/** @type HarperPlugin
	 * @private */
	plugin;

	/** @type Record<string, any> */
	settings;

	/** @type Record<string, string> */
	descriptions;

	/** @param {App} app
	 * @param {HarperPlugin} plugin  */
	constructor(app, plugin) {
		super(app, plugin);
		this.plugin = plugin;

		this.updateDescriptions();
		this.updateSettings();
	}

	updateSettings() {
		this.plugin.getSettings().then((v) => (this.settings = v));
	}

	updateDescriptions() {
		this.plugin.getDescriptions().then((v) => (this.descriptions = v));
	}

	display() {
		const { containerEl } = this;
		containerEl.empty();

		console.log(this.settings.lintSettings);

		for (let setting of Object.keys(this.settings.lintSettings)) {
			let value = this.settings.lintSettings[setting];
			let description = this.descriptions[setting];

			new Setting(containerEl)
				.setName(startCase(setting))
				.setDesc(description)
				.addDropdown((dropdown) =>
					dropdown
						.addOption('default', 'Default')
						.addOption('enable', 'On')
						.addOption('disable', 'Off')
						.setValue(valueToString(value))
						.onChange(async (value) => {
							this.settings.lintSettings[setting] = stringToValue(value);
							await this.plugin.setSettings(this.settings);
						})
				);
		}
	}
}

/** @param {boolean | undefined} value
 * @returns {string} */
function valueToString(value) {
	switch (value) {
		case true:
			return 'enable';
		case false:
			return 'disable';
		case null:
			return 'default';
	}

	throw 'Fell through case';
}

/** @param {str} value
 * @returns {boolean | undefined} */
function stringToValue(str) {
	switch (str) {
		case 'enable':
			return true;
		case 'disable':
			return false;
		case 'default':
			return null;
	}

	throw 'Fell through case';
}
