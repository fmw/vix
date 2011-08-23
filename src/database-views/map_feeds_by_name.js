function(doc) {
    if(doc.type === "feed") {
        emit(doc.name, doc);
    }
}