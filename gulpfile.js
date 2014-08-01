'use strict'

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , runSequence = require('run-sequence')
  ;

var paths = {
    src: 'src/**/*.purs',
    bowerSrc: [
      'bower_components/purescript-*/src/**/*.purs',
      'bower_components/purescript-*/src/**/*.purs.hs'
    ],
    dest: '',
    docs: {
        'Data.Argonaut': {
            dest: 'src/Data/README.md',
            src: 'src/Data/Argonaut.purs'
        },
        'Data.Argonaut.*': {
            dest: 'src/Data/Argonaut/README.md',
            src: 'src/Data/Argonaut/*.purs'
        }
    }
};

var options = {};

function compile (compiler) {
    return function() {
        var psc = compiler(options);
        psc.on('error', function(e) {
            console.error(e.message);
            psc.end();
        });
        return gulp.src([paths.src].concat(paths.bowerSrc))
            .pipe(psc)
            .pipe(gulp.dest(paths.dest));
    }
};

function docs (target) {
    return function() {
        var docgen = purescript.docgen();
        docgen.on('error', function(e) {
            console.error(e.message);
            docgen.end();
        });
        return gulp.src(paths.docs[target].src)
            .pipe(docgen)
            .pipe(gulp.dest(paths.docs[target].dest));
    }
}

function sequence () {
    var args = [].slice.apply(arguments);
    return function() {
        runSequence.apply(null, args);
    }
}

gulp.task('browser', compile(purescript.psc));
gulp.task('make', compile(purescript.pscMake));

gulp.task('docs-Data.Argonaut', docs('Data.Argonaut'));
gulp.task('docs-Data.Argonaut.*', docs('Data.Argonaut.*'));

gulp.task('docs', ['docs-Data.Argonaut', 'docs-Data.Argonaut.*']);

gulp.task('watch-browser', function() {
    gulp.watch(paths.src, sequence('browser', 'docs'));
});

gulp.task('watch-make', function() {
    gulp.watch(paths.src, sequence('make', 'docs'));
});

gulp.task('default', sequence('make', 'docs'));
