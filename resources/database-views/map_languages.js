function(doc) {
    if(doc.type === "feed") {
        emit(doc.language, null);
    }
}
