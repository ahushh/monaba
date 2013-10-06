#!/usr/bin/perl
use v5.12;
####################################################
my $user = "ahushh";
my $host = "192.96.206.74";
my $path = "/home/ahushh/Monaba/dist/build/Monaba/";
my $key  = "~/notes/passwords/haibane_key_nopas";
####################################################
die "usage: ./upload.pl [version] [build|pack|upload|all]" unless defined $ARGV[0] && defined $ARGV[1];
my $v = $ARGV[0];
my $_ = $ARGV[1];
my $build  = /build|all/   ? "cabal clean&&yesod configure&&yesod build" : 'echo "skip building"';
my $pack   = /pack|all/    ? "cd dist/build/Monaba && strip Monaba && cp Monaba Monaba-$v && apack Monaba-$v.7z Monaba-$v" : 'echo "skip packing"';
my $upload = (/upload/     ? "cd dist/build/Monaba &&" : "") .
             (/upload|all/ ? "scp -i $key Monaba-$v.7z ${user}\@${host}:${path}" : 'echo "skip uploading"');
system "$build && $pack && $upload";
