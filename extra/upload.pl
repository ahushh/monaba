#!/usr/bin/perl
use v5.12;
####################################################
my $user = "ahushh";
my $host = "haibane.ru";
my $path = "/home/ahushh/haibane/Monaba/dist/build/Monaba/";
my $key  = "$ENV{HOME}/.ssh/id_rsa";
#systemd
my $start_cmd = "systemctl --user start monaba";
my $stop_cmd = "systemctl --user stop monaba";
#openrc
#my $start_cmd = "sudo /etc/init.d/monaba start";
#my $stop_cmd = "sudo /etc/init.d/monaba stop";
####################################################
die "usage: ./upload.pl [version] [build|pack|upload|restart|clean|all]" unless defined $ARGV[0] && defined $ARGV[1];
my $v = $ARGV[0];
my $_ = $ARGV[1];
my $build  = /build|all/    ? "cabal clean && yesod build" : 'echo "skip building"';
my $pack   = /pack|all/     ? "cd dist/build/Monaba && strip Monaba && cp Monaba Monaba-$v && apack Monaba-$v.7z Monaba-$v" : 'echo "skip packing"';
my $upload = (/upload/      ? "cd dist/build/Monaba &&" : "") .
             (/upload|all/  ? "scp ". ($key ? "-i $key" : "") ." Monaba-$v.7z ${user}\@${host}:${path}" : 'echo "skip uploading"');
my $restart= (/restart/     ? "cd dist/build/Monaba &&" : "") .
             (/restart|all/ ? "ssh ". ($key ? "-i $key" : "") ." -t ${user}\@${host} 'cd $path && aunpack Monaba-$v.7z && $stop_cmd && cp Monaba-$v Monaba && $start_cmd && echo done'" : 'echo "skip restarting"');
my $clean =  (/clean|all/   ? "ssh -i $key -t ${user}\@${host} 'cd $path && rm Monaba-$v.7z Monaba-$v && echo done cleaning'" : 'echo "skip cleaning"');
system "$build && $pack && $upload && $restart && $clean";
