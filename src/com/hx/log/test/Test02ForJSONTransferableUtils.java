/**
 * file name : Test02ForJSONTransferableUtils.java
 * created at : 5:04:23 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.test;

import java.sql.Connection;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

import com.hx.log.log.Log;
import com.hx.log.util.Constants;
import com.hx.log.util.JSONTransferable;
import com.hx.log.util.JSONTransferableUtils;
import com.hx.log.util.Tools;

public class Test02ForJSONTransferableUtils {

	// ≤‚ ‘JSONTransferableUtils
	public static void main(String[] args) throws Exception {
		
		Log.log.logPatternChain = Constants.justPrintMsgLogPattern;
		
		Log.log(JSONTransferableUtils.generateIdxes("Tools", User.class, 3, "user_") );
	
//		Log.log(JSONTransferableUtils.generateDaoDaoImpl(User.class, JSONTransferableUtils.TYPE_MYSQL) );
		Log.log(JSONTransferableUtils.generateAllDaoDaoImpl(User.class) );
		
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
		private User anotherUser;
		private User anotherUserNotNull = new User();
		private Map<String, String> docs;

	}
	
}
