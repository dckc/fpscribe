module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs.hs"
    ],

    clean: {
      tests: ["tmp"],
      lib: ["js", "externs"]
    },

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],

    psc: {
      tests: {
        options: {
          module: ["Footpedal"],
          main: "Footpedal"
        },
        src: ["<%=libFiles%>"],
        dest: "js/Main.js"
      }
    },

    execute: {
      tests: {
        src: "tmp/tests.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");

  grunt.registerTask("test", ["clean:tests", "psc:tests", "execute:tests"]);
  grunt.registerTask("make", ["pscMake", "dotPsci"]);
  grunt.registerTask("default", ["test", "make"]);
};
