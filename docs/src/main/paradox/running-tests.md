# Running tests

While test cases are provided, they are not automatically run by CI due
to captchas costing money to solved.  
If you'd like to run the tests yourself, you will need the following environmental variables:

| Environmental Variable | Description                                                                                                              |
|------------------------|--------------------------------------------------------------------------------------------------------------------------|
| TWOCAPTCHA_API_KEY     | 2Captcha's [api key](https://2captcha.com/2captcha-api#solving_captchas)                                                 |
| CAPMONSTER_API_KEY     | CapMonster's [client key](https://zennolab.atlassian.net/wiki/spaces/APIS/pages/393308/createTask+captcha+task+creating) |

And then simply run the following command:
```
stack test
```
