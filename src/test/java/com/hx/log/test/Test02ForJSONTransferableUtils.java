/**
 * file name : Test02ForJSONTransferableUtils.java
 * created at : 5:04:23 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.test;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import com.hx.log.util.Constants;
import com.hx.log.json.JSONTransferableUtils;
import com.hx.log.util.Log;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;

import static com.hx.log.util.Log.info;

public class Test02ForJSONTransferableUtils {

	// ≤‚ ‘JSONTransferableUtils
	@Test
	public void testJSONTransferableUtils() throws Exception {
		
		Log.log.setLogPattern(Constants.JUST_PRINT_MSG_LOG_PATTERN);
		
//		Log.log(JSONTransferableUtils.generateIdxes("Tools", User.class, 3, "user_") );
		Log.log(JSONTransferableUtils.generateIdxes(User.class, 3, "user_") );
	
//		Log.log(JSONTransferableUtils.generateDaoDaoImpl(User.class, JSONTransferableUtils.TYPE_MYSQL) );
//		Log.log(JSONTransferableUtils.generateAllDaoDaoImpl(User.class) );
		
		Log.log(JSONTransferableUtils.IDX_MAP_MANAGER);
		Log.log(JSONTransferableUtils.UTILS);

		JSONObject obj = new JSONObject();
		obj.element("name", "hx").element("age", 22).element("friends", new JSONArray().element("123").element("dd"));
		Log.log(obj.toString(4));

	}
	
	// bean
	static class User {
		private String id;
		private String name;
		private String pwd;
		private int abc;
		private Integer abcd;
		private double dbc;
		private Double dbcd;
		private JSONObject userObj;
		private JSONArray userArr;
		private List<User> friendParents;
		private Set<User> niceJob;
		private User[] niceJob02;
		private User anotherUser;
		private User anotherUserNotNull = new User();
		private Map<String, String> docs;
		
	}
}
