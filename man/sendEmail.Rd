\name{sendEmail}
\alias{sendEmail}
\title{Send a Email to My 163 Emailbox}
\description{
  Send a email to my 163 emailbox, which can include attchments.
}
\usage{
  sendEmail(files=NULL)
}
\arguments{
  \item{files}{an character vector, specify the names of files included in attachments, such as 'data.Rdata', 'x.doc' and so on.}
}
\details{
  Nothing.
}
\value{
  No value returned. Only send an email to my 163 box.
}
\author{
Liu Wei
}
\note{
nothing
}
\seealso{
send.mail
}
\examples{
sendEmail()
#
sendEmail(files=list.files()[1])
}
\keyword{Email}
