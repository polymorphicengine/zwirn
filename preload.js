const { ipcRenderer } = require('electron')

window.electronAPI = {
  putInStore : (key, value) => ipcRenderer.send('store', key, value),
  getFromStore : (key) => ipcRenderer.invoke('get-store', key),
  clearStore: () => ipcRenderer.send('clear-store')
};
