class Socket {
  constructor() {
    console.log("Setting up socket");
    this.timer = undefined;
    this.tries = 0;
    this.msgs = this.msgs || [];
    this.socket = new WebSocket("ws://localhost:8081/socket");
    this.socket.addEventListener('open', () => this.processMsgs());
    this.socket.addEventListener('message', console.log);
    this.socket.addEventListener('error', console.log);
    this.socket.addEventListener('close', (e) => {
      console.log('Socket closing', e);
      this.constructor();
    });
  }

  send(msg) {
    console.log("Sending:", msg);
    (this.socket.readyState === 1)
    ? this.socket.send(msg)
    : this.msgs.push(msg);
  }

  // returns the event and the callback function to be saved so they can later be
  // passed to the unsubscribe function
  subscribe(event, callback) {
    this.socket.addEventListener(event, callback);
    return [event, callback];
  }

  // Takes a list with the stored details and removes the event listener
  unsubscribe(ref) {
    this.socket.removeEventListener(...ref);
  }

  // Consumes a queue of messages that is stored when the socket is not ready
  // to send messages
  processMsgs() {
    console.log(this);
    let msg = this.msgs.shift();
    if (msg) {
      this.send(msg);
      this.processMsgs();
    }
  }
}

export default new Socket();
