const keyMap = "default" // possible options: "default", "emacs", "vim", "sublime"

const extraKeys = {
	     "Ctrl-.": hush,
		   "Ctrl-Enter": evalB,
		   "Shift-Enter": evaluateLine,
		   "Ctrl-S": saveFile,
		   "Ctrl-O": loadFile,
		   "Ctrl-A": addEditor,
		   "Ctrl-B": removeEditor,
		   "Ctrl-/": 'toggleComment',
		   "Shift-Ctrl--": increaseFontSize,
		   "Ctrl--": decreaseFontSize,
		   "Ctrl-Up": swapLineUp,
		   "Ctrl-Down": swapLineDown,
		   "Shift-Ctrl-D": duplicateLine,
		   "Shift-Ctrl-H": hideAll,
	     }


const editorSettings = {lineNumbers: true,
			 mode: "haskell",
			 theme: "theme",
			 keyMap: keyMap,
			 extraKeys: extraKeys,
			 matchBrackets: true,
			 autoCloseBrackets: true,
			 theme: "tomorrow-night-eighties"
			 }