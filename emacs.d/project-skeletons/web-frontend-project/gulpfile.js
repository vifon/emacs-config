'use strict';

const gulp = require('gulp');
const sass = require('gulp-ruby-sass');
const rm = require('gulp-rm');
const cp = require('gulp-copy');
const autoprefixer = require('gulp-autoprefixer');
const cleancss = require('gulp-clean-css');
const webpack = require('webpack-stream');
const concat = require('gulp-concat');
const gutil = require('gulp-util');

var paths = {
  html: ['app/*.html'],
  scripts: ['app/*.js'],
  styles: ['app/*.scss'],
  dest: {
    html: '.',
    scripts: 'dist/js/',
    styles: 'dist/css/',
    all: ['dist/**', '*.html'],
  },
};

function swallowError (error) {
  console.log(error.toString());
  this.emit('end');
}

gulp.task('html', function() {
  return gulp.src(paths.html)
    .pipe(gulp.dest(paths.dest.html));
});

gulp.task('styles', function() {
  return sass(paths.styles, { style: 'expanded' })
    .pipe(autoprefixer({browsers: ['last 2 versions']}))
    .pipe(concat("style.css"))
    .pipe(gutil.env.env === 'prod' ? cleancss() : gutil.noop())
    .pipe(gulp.dest(paths.dest.styles));
});

gulp.task('scripts', function() {
  var webpack_conf = require('./webpack.config.js');

  return gulp.src(paths.scripts)
    .pipe(webpack(webpack_conf))
    .on('error', swallowError)
    .pipe(gulp.dest(paths.dest.scripts));
});

gulp.task('clean', function() {
  return gulp.src(paths.dest.all).pipe(rm());
});

gulp.task('watch', function() {
  gulp.watch(paths.styles, ['styles']);
  gulp.watch(paths.scripts, ['scripts']);
  gulp.watch(paths.html, ['html']);
});

gulp.task('default', ['html', 'styles', 'scripts']);
