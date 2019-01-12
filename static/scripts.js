// File designed to be as "jslint.com clean" as possible

var mud = {}; // Namespace

(function () {
	"use strict";

	// Constants
	mud.ENTRY_ID = 'entry';
	mud.OUTPUT_ID = 'output';
	mud.POLLING_FREQUENCY = 5000;

	// Global variables
	mud.user = null;
	mud.pollingHandle = -1;

	mud.initPage = function () {
		document.getElementById(mud.ENTRY_ID).focus();

		mud.writeOutput('Welcome to HaskellMUD!');

		var userHash = self.document.location.hash;
		if (null === userHash || '' === userHash) {
			mud.user = prompt('Enter your username (append #username to URL to avoid this prompt)');
		} else {
			mud.user = userHash.substring(1);
		}

		mud.pollingHandle = window.setInterval(function () { mud.processEntry(''); }, mud.POLLING_FREQUENCY);
	};

	mud.onEntryKeyPress = function (oCtl, oEvent) {
		if (mud.isEnterKeyPress(oEvent)) {
			// Capture the current text as a command
			var sEntry = oCtl.value;

			// Reset the text entry for the next command
			oCtl.value = '';

			// Process the entrt
			mud.processEntry(sEntry);
		}
	};

	mud.isEnterKeyPress = function (oEvent) {
		var keynum;

		if (window.event) { // IE8 and earlier
			keynum = oEvent.keyCode;
		} else if (oEvent.which) { // IE9/Firefox/Chrome/Opera/Safari
			keynum = oEvent.which;
		}

		// Detect ENTER key
		return ('\n' === String.fromCharCode(keynum) || '\r' === String.fromCharCode(keynum));
	};

	mud.processEntry = function (sEntry) {
		var oAjaxRequest, sResponse, sOutput;
		oAjaxRequest = new XMLHttpRequest();

		// Synchronous Ajax for simplicity
		oAjaxRequest.open("POST", "action", false);
		oAjaxRequest.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
		oAjaxRequest.send('uid=' + mud.user + '&cmd=' + encodeURIComponent(sEntry));

		sResponse = oAjaxRequest.responseText;
		if ('' !== sEntry || '' !== sResponse) {
			sOutput = '';
			if ('' !== sEntry) {
				sOutput += '> ' + sEntry;
			}

			if ('' !== sResponse) {
				if ('' !== sOutput) {
					sOutput += '\n';
				}

				sOutput += sResponse;
			}

			// Write both our input and the response to the output area 
			mud.writeOutput(sOutput);
		}
	};

	mud.writeOutput = function (sOutput) {
		var oOutput, sPadding;
		oOutput = document.getElementById(mud.OUTPUT_ID);

		// Get a spacer unles we are the first entry
		sPadding = '\n';
		if (oOutput.value.length === 0) {
			sPadding = '';
		}

		// Append the output to the text area
		oOutput.value += sPadding + sOutput;

		// Scroll the text into view
		oOutput.scrollTop = oOutput.scrollHeight;
	};
}());
