context("read-functions")

test_that(".readFlexmapCsv works with replicates and dilutions", {
csv <- "Sample;Replicate;Dilution;F1;F2;F3;F4;F5
A;1;10;1;2;3;4;5
A;2;10;6;7;8;9;10
B;1;10;11;12;13;14;15
B;2;10;16;17;18;19;20
C;1;10;21;22;23;24;25
C;2;10;26;27;28;29;30
A;1;100;10;20;30;40;50
A;2;100;60;70;80;90;100
B;1;100;110;120;130;140;150
B;2;100;160;170;180;190;200
C;1;100;210;220;230;240;250
C;2;100;260;270;280;290;300
A;1;1000;100;200;300;400;500
A;2;1000;600;700;800;900;1000
B;1;1000;1100;1200;1300;1400;1500
B;2;1000;1600;1700;1800;1900;2000
C;1;1000;2100;2200;2300;2400;2500
C;2;1000;2600;2700;2800;2900;3000
"

  a <- flexmap:::.readFlexmapCsv(textConnection(csv))

  expect_equal(dim(a), c(5, 3, 3, 2))
  expect_equal(dimnames(a),
               list(paste0("F", 1:5), LETTERS[1:3],
               as.character(10^(1:3)), as.character(1:2)))
  expect_equal(as.vector(a[,1,1,1]), 1:5)
  expect_equal(as.vector(a[,1,1,2]), 6:10)
  expect_equal(as.vector(a[,1,2,1]), c(10, 20, 30, 40, 50))
  expect_equal(as.vector(a[,2,1,1]), 11:15)
  expect_equal(as.vector(a[,2,1,2]), 16:20)
  expect_equal(as.vector(a[,3,1,1]), 21:25)
  expect_equal(as.vector(a[,3,1,2]), 26:30)
})

test_that(".readFlexmapCsv keeps the order of features", {
csv <- "Sample;Replicate;Dilution;F3;F2;F1;F4;F5
A;1;10;1;2;3;4;5
A;2;10;6;7;8;9;10
B;1;10;11;12;13;14;15
B;2;10;16;17;18;19;20
C;1;10;21;22;23;24;25
C;2;10;26;27;28;29;30
A;1;100;10;20;30;40;50
A;2;100;60;70;80;90;100
B;1;100;110;120;130;140;150
B;2;100;160;170;180;190;200
C;1;100;210;220;230;240;250
C;2;100;260;270;280;290;300
A;1;1000;100;200;300;400;500
A;2;1000;600;700;800;900;1000
B;1;1000;1100;1200;1300;1400;1500
B;2;1000;1600;1700;1800;1900;2000
C;1;1000;2100;2200;2300;2400;2500
C;2;1000;2600;2700;2800;2900;3000
"

  a <- flexmap:::.readFlexmapCsv(textConnection(csv))

  expect_equal(dim(a), c(5, 3, 3, 2))
  expect_equal(dimnames(a),
               list(c("F3", "F2", "F1", "F4", "F5"), LETTERS[1:3],
               as.character(10^(1:3)), as.character(1:2)))
  expect_equal(as.vector(a[,1,1,1]), 1:5)
  expect_equal(as.vector(a[,1,1,2]), 6:10)
  expect_equal(as.vector(a[,1,2,1]), c(10, 20, 30, 40, 50))
  expect_equal(as.vector(a[,2,1,1]), 11:15)
  expect_equal(as.vector(a[,2,1,2]), 16:20)
  expect_equal(as.vector(a[,3,1,1]), 21:25)
  expect_equal(as.vector(a[,3,1,2]), 26:30)
})

test_that(".readFlexmapCsv works without replicates", {
csv <- "Sample;Dilution;F1;F2;F3;F4;F5
A;10;1;2;3;4;5
B;10;11;12;13;14;15
C;10;21;22;23;24;25
A;100;10;20;30;40;50
B;100;110;120;130;140;150
C;100;210;220;230;240;250
A;1000;100;200;300;400;500
B;1000;1100;1200;1300;1400;1500
C;1000;2100;2200;2300;2400;2500
"

  a <- flexmap:::.readFlexmapCsv(textConnection(csv))

  expect_equal(dim(a), c(5, 3, 3, 1))
  expect_equal(dimnames(a),
               list(paste0("F", 1:5), LETTERS[1:3],
               as.character(10^(1:3)), as.character(1)))
  expect_equal(as.vector(a[,1,1,1]), 1:5)
  expect_equal(as.vector(a[,1,2,1]), c(10, 20, 30, 40, 50))
  expect_equal(as.vector(a[,2,1,1]), 11:15)
  expect_equal(as.vector(a[,3,1,1]), 21:25)
})

test_that(".readFlexmapCsv works without dilutions", {
csv <- "Sample;Replicate;F1;F2;F3;F4;F5
A;1;1;2;3;4;5
A;2;6;7;8;9;10
B;1;11;12;13;14;15
B;2;16;17;18;19;20
C;1;21;22;23;24;25
C;2;26;27;28;29;30
"

  a <- flexmap:::.readFlexmapCsv(textConnection(csv))

  expect_equal(dim(a), c(5, 3, 1, 2))
  expect_equal(dimnames(a),
               list(paste0("F", 1:5), LETTERS[1:3],
               as.character(1), as.character(1:2)))
  expect_equal(as.vector(a[,1,1,1]), 1:5)
  expect_equal(as.vector(a[,1,1,2]), 6:10)
  expect_equal(as.vector(a[,2,1,1]), 11:15)
  expect_equal(as.vector(a[,2,1,2]), 16:20)
  expect_equal(as.vector(a[,3,1,1]), 21:25)
  expect_equal(as.vector(a[,3,1,2]), 26:30)
})

test_that(".readFlexmapCsv works without replicates and dilutions", {
csv <- "Sample;F1;F2;F3;F4;F5
A;1;2;3;4;5
B;11;12;13;14;15
C;21;22;23;24;25
"

  a <- flexmap:::.readFlexmapCsv(textConnection(csv))

  expect_equal(dim(a), c(5, 3, 1, 1))
  expect_equal(dimnames(a),
               list(paste0("F", 1:5), LETTERS[1:3],
               as.character(1), as.character(1)))
  expect_equal(as.vector(a[,1,1,1]), 1:5)
  expect_equal(as.vector(a[,2,1,1]), 11:15)
  expect_equal(as.vector(a[,3,1,1]), 21:25)
})

test_that(".readFlexmapCsv works w/o replicate column but with replicates", {
csv <- "Sample;F1;F2;F3;F4;F5
A;1;2;3;4;5
A;6;7;8;9;10
B;11;12;13;14;15
B;16;17;18;19;20
C;21;22;23;24;25
C;26;27;28;29;30
"

  a <- flexmap:::.readFlexmapCsv(textConnection(csv))

  expect_equal(dim(a), c(5, 3, 1, 2))
  expect_equal(dimnames(a),
               list(paste0("F", 1:5), LETTERS[1:3],
               as.character(1), as.character(1:2)))
  expect_equal(as.vector(a[,1,1,1]), 1:5)
  expect_equal(as.vector(a[,1,1,2]), 6:10)
  expect_equal(as.vector(a[,2,1,1]), 11:15)
  expect_equal(as.vector(a[,2,1,2]), 16:20)
  expect_equal(as.vector(a[,3,1,1]), 21:25)
  expect_equal(as.vector(a[,3,1,2]), 26:30)
})

test_that(".readFlexmapCsv works w/o replicate column but with replicates and dilutions", {
csv <- "Sample;Dilution;F1;F2;F3;F4;F5
A;10;1;2;3;4;5
A;10;6;7;8;9;10
B;10;11;12;13;14;15
B;10;16;17;18;19;20
C;10;21;22;23;24;25
C;10;26;27;28;29;30
A;100;10;20;30;40;50
A;100;60;70;80;90;100
B;100;110;120;130;140;150
B;100;160;170;180;190;200
C;100;210;220;230;240;250
C;100;260;270;280;290;300
A;1000;100;200;300;400;500
A;1000;600;700;800;900;1000
B;1000;1100;1200;1300;1400;1500
B;1000;1600;1700;1800;1900;2000
C;1000;2100;2200;2300;2400;2500
C;1000;2600;2700;2800;2900;3000
"

  a <- flexmap:::.readFlexmapCsv(textConnection(csv))

  expect_equal(dim(a), c(5, 3, 3, 2))
  expect_equal(dimnames(a),
               list(paste0("F", 1:5), LETTERS[1:3],
               as.character(10^(1:3)), as.character(1:2)))
  expect_equal(as.vector(a[,1,1,1]), 1:5)
  expect_equal(as.vector(a[,1,1,2]), 6:10)
  expect_equal(as.vector(a[,1,2,1]), c(10, 20, 30, 40, 50))
  expect_equal(as.vector(a[,2,1,1]), 11:15)
  expect_equal(as.vector(a[,2,1,2]), 16:20)
  expect_equal(as.vector(a[,3,1,1]), 21:25)
  expect_equal(as.vector(a[,3,1,2]), 26:30)
})

test_that(".readPhenoDataCsv", {
csv <- "Sample;Class
A;ClassA
B;ClassA
C;ClassB
D;ClassB
"

  adf <- AnnotatedDataFrame(data.frame(Class=rep(c("ClassA", "ClassB"),
                                                 each=2),
                                       row.names=LETTERS[1:4],
                                       stringsAsFactors=FALSE),
                            dimLabels=c("sampleNames", "sampleColumns"))
  expect_equal(.readPhenoDataCsv(textConnection(csv)), adf)
})

test_that(".readPhenoDataCsv throws an error if duplicated ids exists", {
csv <- "Sample;Class
A;ClassA
A;ClassA
C;ClassB
"
  expect_error(.readPhenoDataCsv(textConnection(csv)),
               "PhenoData file has to have unique sample ids in the first column.")
})
