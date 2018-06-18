# running docker automated tests locally
1. `docker-compose -f docker-compose.test.yml build`
2. `docker-compose -f docker-compose.test.yml up`
3. `# wait for SUT to run test.`
4. `docker-compose -f docker-compose.test.yml rm`


