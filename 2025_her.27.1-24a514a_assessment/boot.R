setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())


TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'caa.txt',
                title='Catch at age',
                period = '1988-2024',
                file = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'caa_cv.txt',
                title='CV data for catch at age',
                period = '1988-2024',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'cw.txt',
                title='catch mean weight',
                period = '1988-2024',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'dw.txt',
                title='discard mean weight',
                period = '1988-2024',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'lf.txt',
                title='landing fraction',
                period = '1988-2024',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'lw.txt',
                title='landing mean weight',
                period = '1988-2025',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'matprop.txt',
                title='proportion of mature',
                period = '1988-2025',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'nm.txt',
                title='natural mortality',
                period = '1988-2025',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'pf.txt',
                title='proportion of f',
                period = '1988-2025',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'pm.txt',
                title='proportion of m',
                period = '1988-2025',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'west.txt',
                title='stock mean weight',
                period = '1988-2025',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'rfid_index_BioSampleID.txt',
                title='Tagging data as index, BiosampleID',
                period = '2017-2022',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'rfid_cv_BioSampleID.txt',
                title='Tagging data as cv, BiosampleID',
                period = '2017-2022',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',
                year = 2025,
                data.files = 'survey_T2025-02-05.txt',
                title='multi survey file',
                period = '1988-2025',
                file = T,append = T)
TAF::draft.data(originator = 'WGWIDE',year = 2025,
                data.files = 'surveycv_T2025-02-05.txt',
                title='cv data for multi survey file',
                period = '1988-2025',
                file = T,append = T)




#Make the bib file for the software used in the repository
#Note: the gridExtra pacage is used, but fail to be installed in TAF.
TAF::draft.software(package = 'ggplot2',file=T,append=F)
TAF::draft.software(package = 'reshape2',file=T,append=T)
TAF::draft.software(package = 'plyr',file=T,append=T)
TAF::draft.software(package = 'quarto',file=T,append=T)
TAF::draft.software(package = 'ggplotify',file=T,append=T)
TAF::draft.software(package = 'patchwork',file=T,append=T)
TAF::draft.software(package = 'bbmle',file=T,append=T)
TAF::draft.software(package = 'ggrepel',file=T,append=T)





# TAF::draft.software(package = 'gridExtra',source='cran/gridExtra@2.3',file=T,append=T)
TAF::draft.software(package = 'stockassessment',
                    version = '0.12.0',
                    source = 'fishfollower/SAM/stockassessment@9941c3b',#For a spesific version, change the @tag here
                    file = T,append = T)

#Need to do this every time the above is changed
TAF::taf.boot()

TAF::source.all()
# TAF::source.taf('data.R')
# TAF::source.taf('model.R')
# TAF::source.taf('output.R')
# TAF::source.taf('report.R')
TAF::source.taf('forecast.R')


