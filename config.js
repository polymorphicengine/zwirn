exports.defaults = {
	tidal: {
      dirtport: 57120,
			latency: 0.1,
      frameTimespan: 1/20,
      processAhead: 3/10,
      link: true,
			skipTicks: 10,
      quantum: 4,
      beatsPerCycle: 4
		},
	editor: {
		lineNumbers: true,
	  mode : "haskell",
	  theme: "tomorrow-night-eighties",
		keyMap: "default",
		matchBrackets: true,
		autoCloseBrackets: true,
		extraKeys: {
	  	"Ctrl-." : "hush",
			"Ctrl-Enter" : "evalBlockAtCursor",
			"Shift-Enter" : "evalLineAtCursor",
      "Shift-Ctrl-Enter" : "evalWhole",
			"Ctrl-S" : "saveFile",
			"Ctrl-O" : "loadFile",
			"Ctrl-A" : "addEditor",
      "Shift-Ctrl-A" : "removeEditor",
      "Ctrl-M" : "toggleHighlight",
			"Ctrl-/" : "toggleComment",
			"Shift-Ctrl--" : "increaseFontSize",
			"Ctrl--" : "decreaseFontSize",
			"Ctrl-Up" : "swapLineUp",
			"Ctrl-Down" : "swapLineDown",
			"Shift-Ctrl-D" : "duplicateLine",
			"Shift-Ctrl-H" : "hideAll"
		}
	},
	bootPath: "",
	highlight: true,
	hydra: true
};

exports.schema = {
	tidal: {
		type: 'object',
		properties: {
      dirtport: {
        type: 'number',
				default: 57120
      },
			latency: {
        type: 'number',
				default: 0.1
      },
      frameTimespan: {
        type: 'number',
				default: 1/20
      },
      processAhead: {
        type: 'number',
        default: 3/10
      },
      link: {
        type: 'boolean',
        default: true
      },
			skipTicks: {
        type: 'number',
        default: 10
      },
      quantum: {
        type: 'number',
        default: 4
      },
      beatsPerCycle: {
        type: 'number',
        default: 4
      }
    }
	},
	editor: {
		type: 'object',
		properties: {
			lineNumbers: {
				type: 'boolean',
				default: true
			},
			mode : {
				type: 'string',
				default: "haskell"
			},
			theme: {
				type: 'string',
				default: "tomorrow-night-eighties"
			},
			matchBrackets: {
				type: 'boolean',
				default: true
			},
			autoCloseBrackets: {
				type: 'boolean',
				default: true
			},
			keyMap: {
				type: 'string',
				default: "default",
				enum: ["default", "vim", "emacs", "sublime"]
			},
			extraKeys: {
				type: 'object',
				properties: {
					"Ctrl-." : {
            type: 'string',
						default: "hush"
					},
					"Ctrl-Enter" : {
            type: 'string',
						default: "evalBlockAtCursor"
					},
					"Shift-Enter" : {
            type: 'string',
						default: "evalLineAtCursor"
					},
					"Shift-Ctrl-Enter" : {
            type: 'string',
						default: "evalWhole"
					},
					"Ctrl-S" : {
            type: 'string',
						default: "saveFile"
					},
					"Ctrl-O" : {
            type: 'string',
						default: "loadFile"
					},
					"Ctrl-A" : {
            type: 'string',
						default: "addEditor"
					},
					"Shift-Ctrl-A" : {
            type: 'string',
						default: "removeEditor"
					},
					"Ctrl-M" : {
            type: 'string',
						default: "toggleHighlight"
					},
					"Ctrl-/" : {
            type: 'string',
						default: "toggleComment"
					},
					"Ctrl-+" : {
            type: 'string',
						default: "increaseFontSize"
					},
					"Ctrl--" : {
            type: 'string',
						default: "decreaseFontSize"
					},
					"Ctrl-Up" : {
            type: 'string',
						default: "swapLineUp"
					},
					"Ctrl-Down" : {
            type: 'string',
						default: "swapLineDown"
					},
					"Shift-Ctrl-D" : {
            type: 'string',
						default: "duplicateLine"
					},
					"Shift-Ctrl-H" : {
            type: 'string',
						default: "hideAll"
					},
				}
			}
		}
	},
	bootPath: {
		type: 'string'
	},
	highlight: {
		type: 'boolean'
	},
	hydra: {
		type: 'boolean'
	}
};
