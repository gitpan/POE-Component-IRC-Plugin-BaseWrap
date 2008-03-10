#!/usr/bin/env perl

use strict;
use warnings;

use POE qw(Component::IRC  Component::IRC::Plugin::Example);

my $irc = POE::Component::IRC->spawn(
    nick        => 'TimeBot',
    server      => 'irc.freenode.net',
    port        => 6667,
    ircname     => 'Time bot',
);

POE::Session->create(
    package_states => [
        main => [ qw(_start irc_001) ],
    ],
);

$poe_kernel->run;

sub _start {
    $irc->yield( register => 'all' );

    $irc->plugin_add(
        'Example' =>
            POE::Component::IRC::Plugin::Example->new
    );

    $irc->yield( connect => {} );
}

sub irc_001 {
    $_[KERNEL]->post( $_[SENDER] => join => '#zofbot' );
}