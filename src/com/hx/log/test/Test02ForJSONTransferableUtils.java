/**
 * file name : Test02ForJSONTransferableUtils.java
 * created at : 5:04:23 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.test;

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

	// 测试JSONTransferableUtils
	public static void main(String[] args) throws Exception {
		
		Log.log.logPatternChain = Constants.justPrintMsgLogPattern;
		
		Log.log(JSONTransferableUtils.generateIdxes("Tools", User.class, 3, "user_") );
	
//		Log.log(JSONTransferableUtils.generateDaoDaoImpl(User.class, JSONTransferableUtils.TYPE_MYSQL) );
		Log.log(JSONTransferableUtils.generateAllDaoDaoImpl(User.class) );
		
		Log.log(JSONTransferableUtils.utils );
		
	}
	
	// bean
	static class User implements JSONTransferable<User, Integer> {
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

		// for debug
		public String toString() {
			return encapJSON(new JSONObject().element(beanKey(), defaultLoadIdx() ), new JSONObject().element(beanKey(), defaultFilterIdx()) ).toString();
		}

		// loadFromObject相关索引
		public static final int CAMEL = 0;
		public static final int UNDER_LINE = CAMEL + 1;
		public static final int PREFIX_CAMEL = UNDER_LINE + 1;
		public static final int PREFIX_UNDER_LINE = PREFIX_CAMEL + 1;
		public static final String[] idIdxes = {"id", "id", "user_id", "user_id" }; 
		public static final String[] nameIdxes = {"name", "name", "user_name", "user_name" }; 
		public static final String[] pwdIdxes = {"pwd", "pwd", "user_pwd", "user_pwd" }; 
		public static final String[] abcIdxes = {"abc", "abc", "user_abc", "user_abc" }; 
		public static final String[] abcdIdxes = {"abcd", "abcd", "user_abcd", "user_abcd" }; 
		public static final String[] dbcIdxes = {"dbc", "dbc", "user_dbc", "user_dbc" }; 
		public static final String[] dbcdIdxes = {"dbcd", "dbcd", "user_dbcd", "user_dbcd" }; 
		public static final String[] userObjIdxes = {"userObj", "user_obj", "user_userObj", "user_user_obj" }; 
		public static final String[] userArrIdxes = {"userArr", "user_arr", "user_userArr", "user_user_arr" }; 
		public static final String[] friendParentsIdxes = {"friendParents", "friend_parents", "user_friendParents", "user_friend_parents" }; 
		public static final String[] niceJobIdxes = {"niceJob", "nice_job", "user_niceJob", "user_nice_job" }; 
		public static final String[] anotherUserIdxes = {"anotherUser", "another_user", "user_anotherUser", "user_another_user" }; 
		public static final String[] anotherUserNotNullIdxes = {"anotherUserNotNull", "another_user_not_null", "user_anotherUserNotNull", "user_another_user_not_null" }; 
		public static final String[] docsIdxes = {"docs", "docs", "user_docs", "user_docs" }; 

		// encapJSON相关filter
		public static final int ALL = 0;
		public static final int FILTER_ID = ALL + 1;
		public static final List<Set<String>> filters = Tools.asList(Tools.asSet(""), Tools.asSet(idIdxes));

		public static final String BEAN_KEY = "user_key";
		public static final User PROTO_BEAN = new User();

		@Override
		public User loadFromJSON(Map<String, Object> obj, Map<String, Integer> idxMap) {
			if(Tools.isEmpty(obj) || Tools.isEmpty(idxMap) || (idxMap.get(BEAN_KEY) == null) ) {
				return this;
			}
			int idx = idxMap.get(BEAN_KEY).intValue();

			this.id = Tools.getString(obj, idx, idIdxes);
			this.name = Tools.getString(obj, idx, nameIdxes);
			this.pwd = Tools.getString(obj, idx, pwdIdxes);
			this.abc = Tools.getInt(obj, idx, abcIdxes);
			this.abcd = Tools.getInt(obj, idx, abcdIdxes);
			this.dbc = Tools.getDouble(obj, idx, dbcIdxes);
			this.dbcd = Tools.getDouble(obj, idx, dbcdIdxes);
			this.userObj = Tools.optJSONObject(obj, idx, userObjIdxes);
			this.userArr = Tools.getJSONArray(obj, idx, userArrIdxes);
			this.anotherUser = (this.anotherUser == null) ? new User().loadFromJSON(Tools.getJSONObject(obj, idx, anotherUserIdxes), idxMap) : this.anotherUser.loadFromJSON(Tools.getJSONObject(obj, idx, anotherUserIdxes), idxMap);
			this.anotherUserNotNull = (this.anotherUserNotNull == null) ? new User().loadFromJSON(Tools.getJSONObject(obj, idx, anotherUserNotNullIdxes), idxMap) : this.anotherUserNotNull.loadFromJSON(Tools.getJSONObject(obj, idx, anotherUserNotNullIdxes), idxMap);
			this.docs = Tools.optJSONObject(obj, idx, docsIdxes);

			JSONArray friendParentsArr = Tools.optJSONArray(obj, idx, friendParentsIdxes);
			this.friendParents = new ArrayList(0);
			if(! Tools.isEmpty(friendParentsArr) ) {
				this.friendParents = new ArrayList(friendParentsArr.size() );
				for(int i=0; i<friendParentsArr.size(); i++) {
					this.friendParents.add(new User().loadFromJSON(Tools.getJSONObject(friendParentsArr, i), idxMap) );
				}
			}

			JSONArray niceJobArr = Tools.optJSONArray(obj, idx, niceJobIdxes);
			this.niceJob = new HashSet();
			if(! Tools.isEmpty(niceJobArr) ) {
				for(int i=0; i<niceJobArr.size(); i++) {
					this.niceJob.add(new User().loadFromJSON(Tools.getJSONObject(niceJobArr, i), idxMap) );
				}
			}

			return this;
		}

		@Override
		public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap) {
			return encapJSON(idxMap, filterIdxMap, new HashSet<Object>() );
		}
		@Override
		public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap, Set<Object> cycleDectector) {
			if(cycleDectector.contains(this) ) {
				return JSONObject.fromObject(Constants.OBJECT_ALREADY_EXISTS).element("id", String.valueOf(id()) );
			}
			cycleDectector.add(this);

			if(Tools.isEmpty(idxMap) || (idxMap.get(BEAN_KEY) == null) ) {
				return null;
			}
			int idx = idxMap.get(BEAN_KEY).intValue();

			JSONArray friendParentsArr = new JSONArray();
			if(! Tools.isEmpty(this.friendParents) ) {
				for(User ele : this.friendParents) {
					friendParentsArr.add((ele == null) ? "null" : ele.encapJSON(idxMap, filterIdxMap, cycleDectector));
				}
			}

			JSONArray niceJobArr = new JSONArray();
			if(! Tools.isEmpty(this.niceJob) ) {
				for(User ele : this.niceJob) {
					niceJobArr.add((ele == null) ? "null" : ele.encapJSON(idxMap, filterIdxMap, cycleDectector));
				}
			}

			JSONObject res = new JSONObject()
				.element(idIdxes[Tools.getIdx(idx, idIdxes)], id).element(nameIdxes[Tools.getIdx(idx, nameIdxes)], name).element(pwdIdxes[Tools.getIdx(idx, pwdIdxes)], pwd)
				.element(abcIdxes[Tools.getIdx(idx, abcIdxes)], abc).element(abcdIdxes[Tools.getIdx(idx, abcdIdxes)], abcd).element(dbcIdxes[Tools.getIdx(idx, dbcIdxes)], dbc)
				.element(dbcdIdxes[Tools.getIdx(idx, dbcdIdxes)], dbcd).element(userObjIdxes[Tools.getIdx(idx, userObjIdxes)], userObj).element(userArrIdxes[Tools.getIdx(idx, userArrIdxes)], userArr)
				.element(friendParentsIdxes[Tools.getIdx(idx, friendParentsIdxes)], friendParentsArr).element(niceJobIdxes[Tools.getIdx(idx, niceJobIdxes)], niceJobArr).element(anotherUserIdxes[Tools.getIdx(idx, anotherUserIdxes)], (anotherUser == null) ? "null" : anotherUser.encapJSON(idxMap, filterIdxMap, cycleDectector))
				.element(anotherUserNotNullIdxes[Tools.getIdx(idx, anotherUserNotNullIdxes)], (anotherUserNotNull == null) ? "null" : anotherUserNotNull.encapJSON(idxMap, filterIdxMap, cycleDectector)).element(docsIdxes[Tools.getIdx(idx, docsIdxes)], docs);

			if(Tools.isEmpty(filterIdxMap) || (filterIdxMap.get(BEAN_KEY) == null) ) {
				return res;
			}
			int filterIdx = filterIdxMap.get(BEAN_KEY).intValue();
			return Tools.filter(res, filters.get(Tools.getIdx(filterIdx, filters.size())) );
		}

		@Override
		public User newInstance(Object... args) {
			return new User();
		}
		@Override
		public String id() {
			return id;
		}
		@Override
		public String beanKey() {
			return BEAN_KEY;
		}
		@Override
		public User protoBean() {
			return PROTO_BEAN;
		}
		@Override
		public Integer defaultLoadIdx() {
			return CAMEL;
		}
		@Override
		public Integer defaultFilterIdx() {
			return ALL;
		}
		@Override
		public User set(String attr, Object val) {
			switch (attr) {
				case "id": 
					this.id = (String) val; 
				case "name": 
					this.name = (String) val; 
				case "pwd": 
					this.pwd = (String) val; 
				case "abcd": 
					this.abcd = (Integer) val; 
				case "dbcd": 
					this.dbcd = (Double) val; 
				case "userObj": 
					this.userObj = (JSONObject) val; 
				case "userArr": 
					this.userArr = (JSONArray) val; 
				case "friendParents": 
					this.friendParents = (List) val; 
				case "niceJob": 
					this.niceJob = (Set) val; 
				case "anotherUser": 
					this.anotherUser = (User) val; 
				case "anotherUserNotNull": 
					this.anotherUserNotNull = (User) val; 
				case "docs": 
					this.docs = (Map) val; 
			}

			return this;
		}

	}
	
}
