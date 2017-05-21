package com.hx.log.test;

import com.hx.log.util.BeanTransferUtils;
import org.junit.Test;

import static com.hx.log.util.Log.info;

/**
 * Test21BeanTransferUtils
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/21/2017 4:40 PM
 */
public class Test21BeanTransferUtils {

    @Test
    public void test01ForBeanTransfer() {

        String transfer = BeanTransferUtils.transferTo(User.class, User02.class);
        info(transfer);
        transfer = BeanTransferUtils.transferListTo(User.class, User02.class);
        info(transfer);

    }


    public static class User {
        public String name;
        public String age;

        public String getName() {
            return name;
        }
        public void setName(String name) {
            this.name = name;
        }
        public String getAge() {
            return age;
        }
        public void setAge(String age) {
            this.age = age;
        }
    }

    public static class User02 {
        public String name;
        public String age;
        public String friends;

        public String getName() {
            return name;
        }
        public void setName(String name) {
            this.name = name;
        }
        public String getAge() {
            return age;
        }
        public void setAge(String age) {
            this.age = age;
        }

        public String getFriends() {
            return friends;
        }

        public void setFriends(String friends) {
            this.friends = friends;
        }
    }


}
