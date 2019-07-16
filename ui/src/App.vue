<template>
  <v-app id="inspire">
    <v-toolbar color="blue-grey" dark fixed app clipped-right>
      <v-toolbar-side-icon @click.stop="drawer = !drawer"></v-toolbar-side-icon>
      <v-toolbar-title>Scores</v-toolbar-title>
    </v-toolbar>
    <v-navigation-drawer v-model="drawer" fixed app>
      <v-list-tile>
        <v-list-tile-action><v-icon>dashboard</v-icon></v-list-tile-action>
        <v-list-tile-content>
          <v-list-tile-title><router-link to="/">Home</router-link></v-list-tile-title>
        </v-list-tile-content>
      </v-list-tile>
    </v-navigation-drawer>
    <v-content>
      <v-container fluid fill-height>
        <router-view></router-view>
      </v-container>
    </v-content>
  </v-app>
</template>

<script>
  import Vue from 'vue';
  import VueRouter from 'vue-router';
  import Home from './components/Home.vue';
  import Season from './components/Season.vue';

  Vue.use(VueRouter);

  const router = new VueRouter({
    mode: 'history',
    base: __dirname,
    routes: [
      { path: '/', component: Home },
      { path: '/season/:id', component: Season },
    ],
  });

  export default {
    router: router,
    data: () => ({
      drawer: null,
      left: false
    }),
    props: {
      source: String
    }
  }
</script>
