module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    files: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs.hs"
    ],

    clean: {
      all: ["tmp", "output"],
    },
 
    pscMake: {
      all: {
        src: "<%=files%>",
        dest: "js/Main.js"
      }
    },

    execute: {
	all: {
            src: "js/Main.js"
	}
    }  
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  
  grunt.registerTask("psci", ["dotPsci"]);
  grunt.registerTask("default", ["pscMake"]);
};
