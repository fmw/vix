function(feed) {
    if(feed.type === "feed" &&
       feed["current-state"] === true &&
       feed.action !== "delete") {
        emit(feed.language, null);
    }
}
