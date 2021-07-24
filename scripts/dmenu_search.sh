#!/bin/bash

browser="chromium"
declare -A searchengine
searchengine[amazon]="https://www.amazon.com/s?k="
searchengine[aur]="https://aur.archlinux.org/packages/?O=0&K="
searchengine[archpkg]="https://archlinux.org/packages/?sort=&q="
searchengine[archwiki]="https://wiki.archlinux.org/index.php?search="
searchengine[bing]="https://www.bing.com/search?q="
searchengine[debianpkg]="https://packages.debian.org/search?suite=default&section=all&arch=any&searchon=names&keywords="
searchengine[duckduckgo]="https://duckduckgo.com/?q="
searchengine[github]="https://github.com/search?q="
searchengine[gitlab]="https://gitlab.com/search?search="
searchengine[google]="https://www.google.com/search?q="
searchengine[googleimages]="https://www.google.com/search?hl=en&tbm=isch&q="
searchengine[googlenews]="https://news.google.com/search?q="
searchengine[hoogle]="https://hoogle.haskell.org/?hoogle="
searchengine[lbry]="https://lbry.tv/$/search?q="
searchengine[odysee]="https://odysee.com/$/search?q="
searchengine[reddit]="https://www.reddit.com/search/?q="
searchengine[sourceforge]="https://sourceforge.net/directory/?q="
searchengine[stackoverflow]="https://stackoverflow.com/search?q="
searchengine[startpage]="https://www.startpage.com/do/dsearch?query="
searchengine[stockquote]="https://finance.yahoo.com/quote/"
searchengine[thesaurus]="https://www.thesaurus.com/misspelling?term="
searchengine[translate]="https://translate.google.com/?sl=auto&tl=en&text="
searchengine[urbandictionary]="https://www.urbandictionary.com/define.php?term="
searchengine[wayback]="https://web.archive.org/web/*/"
searchengine[wikipedia]="https://en.wikipedia.org/wiki/"
searchengine[youtube]="https://www.youtube.com/results?search_query="

if [ -z $1 ]; then
    selected=$(printf '%s\n' "${!searchengine[@]}" | sort | dmenu -l 20 -p 'Choose search engine:') "$@" || exit
else
    selected=$1
fi

url=$(echo "${searchengine["${selected}"]}") || exit
[ -z $selected ] && exit

query="$(echo "" | dmenu -p "$selected:" -l 0 | sed 's/ /+/g')"
[ -z $query ] && exit
$browser "$url$query"
