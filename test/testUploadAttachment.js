exports.someview = (function(doc) {
    if(doc.name && doc.age) {
        emit(doc.name, doc.age);
    }
}).toString()
