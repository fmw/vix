function(doc) {
    if(doc.type === "document") {
        emit(doc.feed, doc);
    }
}
