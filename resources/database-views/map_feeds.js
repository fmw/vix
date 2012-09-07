function(feed) {
    if(feed.type === "feed") {
        emit([feed.language, feed.name, feed.datestamp], feed);
    }
}
