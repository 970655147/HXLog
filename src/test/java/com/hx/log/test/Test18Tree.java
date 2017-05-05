package com.hx.log.test;

import com.hx.json.JSONObject;
import com.hx.log.alogrithm.tree.TreeUtils;
import com.hx.log.alogrithm.tree.interf.TreeIdExtractor;
import com.hx.log.alogrithm.tree.interf.TreeInfoExtractor;
import com.hx.log.util.Tools;
import org.junit.Test;

import java.util.List;

import static com.hx.log.util.Log.info;

/**
 * Test18Tree
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 9:39 PM
 */
public class Test18Tree {

    @Test
    public void test01GenerateTree() {

        List<User> ls = Tools.asList(
          new User("1", null), new User("2", "1")
        );

        JSONObject root = TreeUtils.generateTree(ls, new TreeInfoExtractor<User>() {
            @Override
            public void extract(User bean, JSONObject obj) {
                obj.put("id", bean.id);
                obj.put("parentId", bean.parentId);
            }
        });

        info(root.toString() );

        root = TreeUtils.childArrayify(root);
        info(root.toString() );

        info(new JSONObject().toString());

    }

    /**
     * ≤‚ ‘bean
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/5/2017 9:40 PM
     */
    private static class User implements TreeIdExtractor<User, String> {
        public String id;
        public String parentId;

        public User(String id, String parentId) {
            this.id = id;
            this.parentId = parentId;
        }

        @Override
        public String id() {
            return id;
        }

        @Override
        public String parentId() {
            return parentId;
        }
    }

}
