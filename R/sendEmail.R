sendEmail <- function(files=NULL){
  ## the function send eamil to me and tell me the running finished!

  sender = 'weidliu321@163.com' # 发件人 授权码：abcd123456
  recipients <- 'weidliu321@163.com' # 收件人
  title = "Report of Running Code"
  body = "I'm robot cat. Now I inform that the code run has finished and the attachments."
  send.mail(
    from = sender,
    to = recipients,
    subject = title,
    body =body,
    encoding = "utf-8",
    html = TRUE,
    smtp = list(
      host.name = "smtp.163.com",
      port = 465,
      user.name = sender,
      passwd = 'ldw438369d', # 授权码
      ssl = TRUE
    ),
    authenticate = TRUE,
    send = TRUE,
    attach.files = files
  )
}
