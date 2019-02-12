import deriva.core

def get_pathbuilder(host):
    server = deriva.core.DerivaServer("https", host)
    catalog = server.connect_ermrest(2)
    return catalog.getPathBuilder()

def get_relevant_study_urls(pb, host, instance_rid):
    server = deriva.core.DerivaServer("https", host)
    catalog = server.connect_ermrest(2)
    pb = catalog.getPathBuilder()
    application_file_table = pb.RNASeq.Application_File.alias('F')
    role_table = pb.RNASeq.Application_File_Tag.alias('R')
    path = application_file_table.path.\
           filter(application_file_table.Visualization.eq(instance_rid)).\
           link(role_table)
    retval = dict()
    for row in path.entities(path.R.Tag_Name, path.F.File_URL):
        retval[row.get("Tag_Name")] = "https://{h}{p}".format(h=host, p=row.get("File_URL"))
    return(retval)

            
