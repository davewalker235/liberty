<template>
  <div class="score">
    <span>{{ date }}</span>
    <span>{{ team }}</span>
    <input type="checkbox" v-bind:checked="scores === 'committee'" v-on:change="toggleCommittee">
    <span></span>
    <input type="number" v-for="(score, n) in [...scores]" v-bind:key="n" v-bind:value="score" v-on:change="changeScore">
  </div>
</template>

<script>
  import Socket from '../socket.js';

  export default {
    name: 'Score',
    props: ['data'],
    data: function () {
      return this.data;
    },
    computed: {
      key: function() { return this.date + this.team; },
      is_committee: function() { return this.scores === 'committee' }
    },
    methods: {
      changeScore: function() {
        let inputs = this.$el.querySelectorAll('input[type=number]');
        let values = [...inputs].map(i => parseInt(i.value, 10)).filter(i => !isNaN(i));
        this.data.scores = values;
        Socket.send('score:' + JSON.stringify(this.data));
      },
      toggleCommittee: function(e) {
        this.data.scores = e.target.checked ? "committee" : [null];
        Socket.send('score:' + JSON.stringify(this.data));
        this.$forceUpdate();
      }
    }
  }
</script>

<style>
  .score {
    display: flex;
  }

  .score input[type=checkbox]:not(:checked) + span {
    display: none;
  }

  .score input[type=checkbox]:checked + span:before {
    content: "Commitee";
    display: block;
  }

  .score span,
  .score input {
    flex-basis: 3em;
  }

  .score span {
    white-space: nowrap;
    margin-right: 1em;
  }

  .score input {
    width: 3em;
  }
</style>
