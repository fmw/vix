function(doc) {
    if(doc.type === "document") {
        emit([doc.slug, doc.datestamp], doc);
    }
}
