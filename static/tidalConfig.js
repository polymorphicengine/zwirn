const commands = {
	          hush: hush,
	          evalBlockAtCursor: evalBlockAtCursor,
	          evalLineAtCursor: evalLineAtCursor,
	          evalWhole: evalWhole,
	          saveFile: saveFile,
	          loadFile: loadFile,
	          addEditor: addEditor,
	          removeEditor: removeEditor,
	          toggleHighlight: toggleHighlight,
	          increaseFontSize: increaseFontSize,
	          decreaseFontSize: decreaseFontSize,
	          swapLineUp: swapLineUp,
	          swapLineDown: swapLineDown,
	          duplicateLine: duplicateLine,
	          hideAll: hideAll
	         };

CodeMirror.commands = Object.assign(CodeMirror.commands, commands);
