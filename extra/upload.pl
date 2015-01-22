#!/usr/bin/perl
use v5.12;
####################################################
my $user = "user";
my $host = "host.ru";
my $path = "/home/user/Monaba/dist/build/Monaba/";
my $key  = "$ENV{HOME}/notes/passwords/ssh_key";
#systemd
#my $start_cmd = "sudo systemctl start monaba.service";
#my $stop_cmd = "sudo systemctl stop monaba.service";
#openrc
my $start_cmd = "sudo /etc/init.d/monaba start";
my $stop_cmd = "sudo /etc/init.d/monaba stop";
####################################################
die "usage: ./upload.pl [version] [build|pack|upload|restart|all]" unless defined $ARGV[0] && defined $ARGV[1];
my $v = $ARGV[0];
my $_ = $ARGV[1];
my $build  = /build|all/    ? "cabal clean&&yesod configure&&yesod build" : 'echo "skip building"';
my $pack   = /pack|all/     ? "cd dist/build/Monaba && strip Monaba && cp Monaba Monaba-$v && apack Monaba-$v.7z Monaba-$v" : 'echo "skip packing"';
my $upload = (/upload/      ? "cd dist/build/Monaba &&" : "") .
             (/upload|all/  ? "scp -i $key Monaba-$v.7z ${user}\@${host}:${path}" : 'echo "skip uploading"');
my $restart= (/restart/     ? "cd dist/build/Monaba &&" : "") .
             (/restart|all/ ? "ssh -i $key -t ${user}\@${host} 'cd $path && aunpack Monaba-$v.7z && $stop_cmd && cp Monaba-$v Monaba && $start_cmd && echo done'" : 'echo "skip restarting"');
system "$build && $pack && $upload && $restart";
