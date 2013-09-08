use v5.12;

unless ($ARGV[0] and $ARGV[1] and $ARGV[2] and $ARGV[3]) {
    say "usage: ./$0 [file with a text] [destination sql dump] [index to start with] [lang]";
    say "example: ./make-captcha-dict.pl tales.txt dumpl.sql 1 en";
    exit;
}
`sed -e 's/\\s/\\n/g' -e 's/[0-9,\.»«;:—"!?)(]//g' $ARGV[0] | awk {'print tolower(\$_)'} | grep -v '-'| grep -E '.{2,}' | sort -u > dict.txt`;

open my $fh , '<:utf8', 'dict.txt';
open my $fh1, '>:utf8', $ARGV[1];

my $i = $ARGV[2];
my $lang = $ARGV[3];
while (<$fh>)
{
    s/\s//g;
    if ($_)
    {
        say $fh1 "INSERT INTO `captcha_dict` VALUES ($i,'$_','$lang');";
        $i++;
    }
}
close $fh;
close $fh1;
`rm dict.txt`;
