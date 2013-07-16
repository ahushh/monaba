#!/usr/bin/php
<?php
include_once '/home/user/geshi/geshi.php';
$source = file_get_contents("php://stdin");
$geshi = new GeSHi($source, $argv[1]);
//$geshi->enable_keyword_links(false);
//$geshi->enable_line_numbers(GESHI_FANCY_LINE_NUMBERS);
$geshi->set_overall_style('background-color: #EFEFEF; border: 1px dashed #DFDFDF; padding: 10px;', true);
echo $geshi->parse_code();
?>
    