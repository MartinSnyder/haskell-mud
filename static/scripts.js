var mud = {}; // Namespace

(function () {
	"use strict";

	// Constants
	mud.ENTRY_ID = 'entry';
	mud.OUTPUT_ID = 'output';
	mud.POLLING_FREQUENCY = 2000;
	mud.MAX_IDLE_TICKS = 5 * 60 / (mud.POLLING_FREQUENCY / 1000); // 5 minutes

	// Global variables
	mud.user = null;
	mud.pollingHandle = -1;
	mud.idleTicks = 0;
	mud.previousCommands = [];
	mud.previousCommandsIndex = -1;

	mud.initPage = function () {
		document.getElementById(mud.ENTRY_ID).focus();

		mud.writeOutput('Welcome to HaskellMUD!');
		mud.writeOutput('Enter your username:');

		mud.pollingHandle = window.setInterval(mud.poll, mud.POLLING_FREQUENCY);
	};

	mud.poll = function () {
		mud.idleTicks++;
		if (mud.idleTicks > mud.MAX_IDLE_TICKS) {
			// Stop polling
			window.clearInterval(mud.pollingHandle);
			mud.pollingHandle = -1;

			mud.writeOutput('Disconnected due to inactivity. Refresh the page to reconnect.');
		}
		else {
			mud.processEntry('');
		}
	};

	mud.onEntryKeyPress = function (oCtl, oEvent) {
		if (mud.isEnterKeyPress(oEvent)) {
			mud.submitCommand(oCtl);
		}
		else if(mud.isUpKeyPress(oEvent)) {
			mud.recallNextHistoricalCommand(oCtl);
		}
		else if(mud.isDownKeyPress(oEvent)) {
			mud.recallPrevHistoricalCommand(oCtl);
		}
	};

	mud.submitCommand = function (oCtl) {
		// Capture the current text as a command
		var sEntry = oCtl.value;

		// Reset the text entry for the next command
		oCtl.value = '';

		if (mud.user === null) {
			// Set the username first if we still need one
			if (sEntry.length > 0) {
				mud.user = sEntry;
				mud.writeOutput('Type \'help\' for a list of commands');
			}
		}
		else {
			// Add the command to the beginning of the command history
			mud.previousCommands.unshift(sEntry);
			//  Reset our place in the previous commands list to the beginning
			mud.previousCommandsIndex = -1;

			// Process the entry
			mud.idleTicks = 0;
			mud.processEntry(sEntry);
		}
	};

	mud.recallNextHistoricalCommand = function (oCtl) {
		//  Ensure that we aren't at the end of our historical command list
		if(mud.previousCommandsIndex + 1 <= mud.previousCommands.length - 1)
		{
			mud.previousCommandsIndex++;
			oCtl.value = mud.previousCommands[mud.previousCommandsIndex];
		}
	};

	mud.recallPrevHistoricalCommand = function (oCtl) {
		//  Ensure that we aren't going past the beginning of our historical command list
		if(mud.previousCommandsIndex - 1 >= 0)
		{
			mud.previousCommandsIndex--;
			oCtl.value = mud.previousCommands[mud.previousCommandsIndex];
		}
	};

	mud.isEnterKeyPress = function (oEvent) {
		var keynum = mud.captureKeyNum(oEvent);

		// Detect ENTER key
		return ('\n' === String.fromCharCode(keynum) || '\r' === String.fromCharCode(keynum));
	};

	mud.isUpKeyPress = function (oEvent) {
		var keynum = mud.captureKeyNum(oEvent);

		// Detect UP key
		return (38 == keynum);
	};

	mud.isDownKeyPress = function (oEvent) {
		var keynum = mud.captureKeyNum(oEvent);

		// Detect DOWN key
		return (40 == keynum);
	};

	mud.captureKeyNum = function (oEvent) {
		var result;

		if (window.event) { // IE8 and earlier
			result = oEvent.keyCode;
		} else if (oEvent.which) { // IE9/Firefox/Chrome/Opera/Safari
			result = oEvent.which;
		}

		return result;
	}

	mud.processEntry = function (sEntry) {
		// Early return if no user yet
		if (null === mud.user) {
			return;
		}

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

		// Get a spacer unless we are the first entry
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
