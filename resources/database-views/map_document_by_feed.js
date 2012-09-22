function(doc) {
    if(doc.type === "document" &&
       doc["current-state"] === true &&
       doc.action !== "delete") {
        emit([[doc.language, doc.feed], doc.published], doc);
    }
}
