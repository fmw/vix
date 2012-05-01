function(doc) {
    if(doc.type === "document" && doc["end-time-rfc3339"]) {
        emit([[doc.language, doc.feed], doc["end-time-rfc3339"]], doc);
    }
}
