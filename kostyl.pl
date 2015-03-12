use v5.12;
use Digest::MD5;
`mkdir static/upload/0`;
`find static/files -type f | xargs -n1 -i cp "{}" static/upload/0/`;
`rm -fr static/thumb/*`;
my $md5 = Digest::MD5->new;
my $thumbsize = 200;
for (<static/upload/0/*>) {
    open my $fh, '<', $_;
    /\.(\w+)$/; my $suff = ($1 eq "webm" ? "png" : $1);
    my $d = $md5->addfile($fh)->hexdigest;
    `convert -thumbnail x$thumbsize $_ static/thumb/${thumbsize}thumb-$d.$suff`;
    close $fh;
}
