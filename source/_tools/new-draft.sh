#!/bin/bash
# Creates a draft for a new post

echo -n "Enter a filename for the draft (e.g. foobar.md): "
read name
name=`echo -n $name | tr -d '\r\n'`
date=`date "+%Y-%m-%d %H:%M:%S %z"`
file=../_drafts/"${name}"

cat > $file <<EOF
---
layout: post
title: ""
date: $date
updated: $date
categories: 
disqus: true
tags: 
---

EOF

$EDITOR $file
