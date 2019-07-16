<template>
  <div class="raceday">
    Raceday
  </div>
</template>

<script>
  import Socket from '../socket.js';

  export default {
    data: function() {
      return {
        season: undefined,
        results: undefined
      }
    },
    created: function () {
      this.$watcher = Socket.subscribe('message', (e) => {
        let path, payload;
        [path, payload] = e.data.split('|', 2);
        if (path !== '/season/' + this.$route.params.id) return;
        let data = JSON.parse(payload);
        console.log(data);
        this.season = data.season;
        this.results = data.results;
      });
      Socket.send('/season/' + this.$route.params.id);
    },
    destroyed: function () {
      Socket.unsubscribe(this.$watcher);
    }
  }

</script>
