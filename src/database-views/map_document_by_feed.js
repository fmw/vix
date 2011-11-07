function(doc) {
    if(doc.type === "document") {
        emit([[doc.language, doc.feed], doc.published], doc);
    }
}
