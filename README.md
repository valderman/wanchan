A sort of flexible Nyaa Torrents client
=======================================

Nyanbda is a command line Nyaa Torrents client specifically geared towards
downloading and watching anime series. It has the following features:

* Search and download episodes or whole series.
* Use NyaaTorrents or arbitrary RSS feeds as episode sources.
* Filter search results on season, episode, release group and other criteria.
* Automatic ranking of result coherence: prefer results with better metadata.
* Grouping of related episodes: get the whole series from the same group,
  in the same format.
* Seen episodes cache: don't download episodes you already have.
* Shell integration: run arbitrary commands over downloaded episodes.
* Batch mode: perform a whole bunch of searches at once.
* Daemon mode: never miss a release again, let nyanbda keep track of new
  episodes.
* Web daemon mode: explore and track new releases using a fancy web interface.


Building and installing
-----------------------

Make sure you have the [Haskell platform](http://haskell.org/platform)
installed and the [Haste compiler](https://haste-lang.org) and check out
the repository.

In he repository root, run `make deps` to install all necessary dependencies.
Then, run `make user-install` to install nyanbda into `~/.local/bin`.
You can also run `make global-install` as root, to
install for all users, or just `make` and grab the resulting binary yourself.


Quickstart
----------

To perform a simple search:

    $ nyanbda assassination classroom

This will yield all hits for Assassion Classroom without any particular
filtering. That's a bit much, so let's restrict it:

    $ nyanbda assassination classroom -s1 -e1..100

This filters out all episodes which are not both from season 1 *and* in the
range of episodes 1 to 100.
By giving a range of episode numbers, we also exclude any releases which lack
episode numbers, such as batch torrents, OP/ED and other related material.

Note that nyanbda automatically suggested to get all episodes from
HorribleSubs, even though judging by our initial, unfiltered, search there are
plenty of other release groups to choose from. By default, nyanbda only chooses
one file per season and episode, and prioritizes files based on whether or not
it's possible to get the entire series from the same source.
If we're just exploring our possibilities, this is too strict. We can instead
tell nyanbda to include *all* results that didn't get filtered out:

    $ nyanbda assassination classroom -s1 -e1..100 --list

This shows us that we have a lot of options to choose from. Maybe we want to
go with FFF instead:

    $ nyanbda assassination classroom -s1 -e1..100 -gFFF

...or maybe we prefer HorribleSubs' 480p version over their 1080p version
for some reason:

    $ nyanbda assassination classroom -s1 -e1..100 -r480p

When we're happy with the results we're getting, we can download all torrents
in one fell swoop by simply adding the `--get` option:

    $ nyanbda assassination classroom -s1 -e1..100 -r480p --get

However, now we end up with a whole bunch of torrent files in our current
working directory - that may not be what we wanted. Instead, we can redirect
the files to a directory of our choosing, for instance a `torrents`
subdirectory:

    $ nyanbda assassination classroom -s1 -e1..100 -r480p --get -o./torrents

This plays nicely together with BitTorrent clients that watch a torrent
directory for new downloads, such as
[Transmission](http://www.transmissionbt.com/): simply use your BitTorrent
client's incoming torrents directory as nyanbda's output directory to start
your downloads right away.

However, not all torrent clients work this way. Instead you may need to run
some command to start downloading new torrents.
Nyanbda still has you covered: you can use the `-x` option to run a shell
command on each downloaded torrent file:

    $ nyanbda assassination classroom -s1 -e1..100 -r480p --get -o./torrents -x'rtorrent %f'

To simplify downloading episodes from several series at the same time, you
can put searches into batch files. We create a file `batch.txt` with the
following contents:

    assassination classroom -s1 -e1..100 -r480p
    yuruyuri -s3 -e1..100 -r1080p

Then we call nyanbda with the `--batch` option, leaving out the filters:

    $ nyanbda --batch batch.txt -o./torrents -x'rtorrent %f'

This will download the first season of Assassination Classroom in 480p, and the
third season of YuruYuri in 1080p to the `torrents` subdirectory, and
then run the `rtorrent` command on each downloaded torrent file.

You can also set nyanbda to watch Nyaa for new episodes for you using
daemon mode:

    $ nyanbda --daemon=60 batch.txt -o./torrents -x'rtorrent %f'

This will cause nyanbda to go into batch mode using the specified file every 60
minutes, as specified by the `60` passed to the `--daemon` option.
However, this will re-download *all* episodes of the series given in
`batch.txt` every 60 minutes. That's probably not what we want.
To avoid this, we can use a "seen" file: a file which records the episodes
nyanbda has already downloaded.

    $ nyanbda --daemon=60 batch.txt -o./torrents -x'rtorrent %f' --database=db.sqlite

This will use the file `db.sqlite` to record episode titles as they are
downloaded. New downloads are not initiated for episodes that are recorded in
this file.

Finally, you can tell nyanbda to start in _web daemon mode_: a web application
which lets you easily explore and track new releases.
This mode accepts the same flags as daemon mode, except that watched series are
stored in the database file and not in separate batch text files:

    $ nyanbda --web-daemon=60 -o./torrents -x'rtorrent %f' --database=db.sqlite

To access the web interface, point your browser to `http://localhost:8888`.
