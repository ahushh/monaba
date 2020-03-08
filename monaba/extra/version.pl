#!/usr/local/bin/perl

use v5.30;
use Cwd qw(abs_path);
use File::Basename qw(dirname);

undef $/;

if (scalar(grep /^major|minor|patch$/, @ARGV) == 0 && !($ARGV[0] eq 'set' && $ARGV[1] ne '')) {
  say <<'END';
Bumps or set Monaba version in configs and footer template.

Example of bumping version: perl Monaba/extra/version.pl major|minor|patch
Example of setting version: perl Monaba/extra/version.pl set 2.7.0
END
  die;
}

my @files = ('package.yaml', 'Monaba.cabal', 'templates/default-layout-wrapper.hamlet');
my $scriptDir = abs_path(dirname($0));
my $monabaDir = "$scriptDir/..";

my $fhIN;
my $fhOUT;

open $fhIN, '<', "$monabaDir/package.yaml" or die "Cannot open $monabaDir/package.yaml for read";
$_ = <$fhIN>;
close $fhIN;

/version: \"(?<major>\d+)\.(?<minor>\d+)\.(?<patch>\d+)\"/;

my %result = %+;
my $current = "$result{major}.$result{minor}.$result{patch}";
$result{$ARGV[0]}++ if $ARGV[0] ne 'set';
my $next = $ARGV[0] eq 'set' ? $ARGV[1] : "$result{major}.$result{minor}.$result{patch}";

for my $name (@files) {
  open $fhIN, '<', "$monabaDir/$name" or die "Cannot open $name for read";
  $_ = <$fhIN>;
  close $fhIN;
  s/$current/$next/g;

  open $fhOUT, '>', "$monabaDir/$name" or die "Cannot open $name for write";
  print $fhOUT $_;
  close $fhOUT;
}

say "Update version from $current to $next";
say "Files updated: ".join(' ', @files);