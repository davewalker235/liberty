<template>
  <div>
    <h1>Seasons</h1>
    <div v-for="season in seasons" :key="season.id">
      <router-link :to="`/season/${season.id}`">{{ season.name }}</router-link>
    </div>
  </div>
</template>

<script>

import Socket from '../socket.js';

export default {
  name: 'Home',
  data: function () {
    return {
      seasons: []
    }
  },
  created: function () {
    this.$watcher = Socket.subscribe('message', (e) => {
      let path, payload;
      [path, payload] = e.data.split('|', 2);
      if (path !== '/season/index') return;
      let data = JSON.parse(payload);
      this.seasons = data;
    });
    Socket.send('/season/index');
  },
  destroyed: function () {
    Socket.unsubscribe(this.$watcher);
  }
}
</script>
