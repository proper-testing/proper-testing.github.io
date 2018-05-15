function noSPAMemail(user, hostname, domain, classname, subjectname){
    document.write('<a href="' + 'mailto:' + user + '@' + hostname + '.'
		   + domain);
    if (subjectname != null)
	document.write('?subject=' + subjectname);
    if (classname != null)
	document.write('" class="' + classname);
    document.write('">' + user + '@' + hostname + '.' + domain + '</a>');
}
