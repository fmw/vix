function(doc) {
    if(doc.type === "feed") {
        emit([doc.language, doc.name], doc);
    }
}
