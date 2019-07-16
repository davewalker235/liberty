<template>
  <div v-if="season" :key="season.id" class="season">
    <h2>{{ season.name }}</h2>
    <table>
      <tr>
        <td>Overall<br>Place</td>
        <td>Team</td>
        <td v-for="n in 4" :key="n">Series {{ n }}</td>
        <td>Total<br>Points</td>
      </tr>
      <tr v-for="(result, n) in results" :key="n">
        <td>{{ n + 1 }}</td>
        <td>{{ result.team.name }}</td>
        <td v-for="(series, n) in result.series" :key="n">{{ series.toFixed(1) }}</td>
        <td v-for="n in 4 - result.series.length" :key="n + 4"></td>
        <td>{{ result.total.toFixed(1) }}</td>
      </tr>
    </table>
    <Score v-for="s in scores" :key="`${s.date}-${s.team}`" :data="s"></Score>
  </div>
</template>

<script>
  import Socket from '../socket.js';
  import Score from './Score.vue';

  export default {
    name: 'Season',
    data: function() {
      return {
        season: undefined,
        results: [],
        scores: []
      }
    },
    components: {
      'Score' : Score ,
    },
    created: function () {
      this.$watcher = Socket.subscribe('message', (e) => {
        let path, payload;
        [path, payload] = e.data.split('|', 2);
        if (path !== this.$route.path) return;
        let data = JSON.parse(payload);
        this.season = data.season;
        this.results = data.results;
        this.scores = data.scores;
      });
      Socket.send('subscribe:' + this.$route.path);
    },
    destroyed: function () {
      Socket.send('unsubscribe:' + this.$route.path);
      Socket.unsubscribe(this.$watcher);
    }
  }

</script>

<style>
  .season td:nth-child(n + 3) {
    text-align: right;
  }
</style>
