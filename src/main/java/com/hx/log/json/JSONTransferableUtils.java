/**
 * file name : JSONTransferableUtils.java
 * created at : 4:54:43 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.json;

import com.hx.common.util.ReflectUtils;
import com.hx.json.JSONObject;
import com.hx.log.util.Constants;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.Map.Entry;

/**
 * ���ɸ�����Class��JSONTransferable������
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 4:17 PM
 */
public final class JSONTransferableUtils {

    // disable constructor
    private JSONTransferableUtils() {
        Tools.assert0("can't instantiate !");
    }

    // ���ݸ�����Classд������ģ��
//	// for debug
//	public String toString() {
//		return encapJSON(Constants.doLoadNormalNothingIdxMap, Constants.doFilterNothingFilterMap ).toString();
//	}
//
//	// loadFromObject�������
//	public static final int NORMAL = 0;
//	public static final String[] nameIdxes = {"name" }; 
//	public static final String[] pwdIdxes = {"pwd" }; 
//	public static final String[] friendsIdxes = {"friends" }; 
//	public static final String[] niceIdxes = {"nice" }; 
//
//	// encapJSON���filter
//	public static final int ALL = 0;
//	public static final List<Set<String>> filters = Tools.asList(Tools.asSet("") );
//
//	public static final String BEAN_KEY = "user_key";
//	public static final User PROTO_BEAN = new User();
//
//	@Override
//	public User loadFromJSON(Map<String, Object> obj, Map<String, Integer> idxMap) {
//		if(Tools.isEmpty(idxMap) || (idxMap.get(BEAN_KEY) == null) ) {
//			return this;
//		}
//		int idx = idxMap.get(BEAN_KEY).intValue();
//
//		this.name = Tools.getString(obj, idx, nameIdxes);
//		this.pwd = Tools.getString(obj, idx, pwdIdxes);
//
//		JSONArray friendsArr = Tools.optJSONArray(obj, idx, friendsIdxes);
//		this.friends = new ArrayList(0);
//		if(! Tools.isEmpty(friendsArr) ) {
//			this.friends = new ArrayList(friendsArr.size() );
//			for(int i=0; i<friendsArr.size(); i++) {
//				this.friends.add(new User().loadFromJSON(Tools.getJSONObject(friendsArr, i), idxMap) );
//			}
//		}
//
//		JSONArray niceArr = Tools.optJSONArray(obj, idx, niceIdxes);
//		this.nice = new HashSet();
//		if(! Tools.isEmpty(niceArr) ) {
//			for(int i=0; i<niceArr.size(); i++) {
//				this.nice.add(new User().loadFromJSON(Tools.getJSONObject(niceArr, i), idxMap) );
//			}
//		}
//
//		return this;
//	}
//
//	@Override
//	public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap) {
//		if(Tools.isEmpty(idxMap) || (idxMap.get(BEAN_KEY) == null) ) {
//			return null;
//		}
//		int idx = idxMap.get(BEAN_KEY).intValue();
//
//		JSONArray friendsArr = new JSONArray();
//		if(! Tools.isEmpty(this.friends) ) {
//			for(User ele : this.friends) {
//				friendsArr.add(ele.encapJSON(idxMap, filterIdxMap) );
//			}
//		}
//
//		JSONArray niceArr = new JSONArray();
//		if(! Tools.isEmpty(this.nice) ) {
//			for(User ele : this.nice) {
//				niceArr.add(ele.encapJSON(idxMap, filterIdxMap) );
//			}
//		}
//
//		JSONObject res = new JSONObject()
//			.element(nameIdxes[Tools.getIdx(idx, nameIdxes)], name).element(pwdIdxes[Tools.getIdx(idx, pwdIdxes)], pwd).element(friendsIdxes[Tools.getIdx(idx, friendsIdxes)], friendsArr)
//			.element(niceIdxes[Tools.getIdx(idx, niceIdxes)], niceArr);
//
//		if(Tools.isEmpty(filterIdxMap) || (filterIdxMap.get(BEAN_KEY) == null) ) {
//			return res;
//		}
//		int filterIdx = filterIdxMap.get(BEAN_KEY).intValue();
//		return Tools.filter(res, filters.get(Tools.getIdx(filterIdx, filters.size())) );
//	}
//
//	@Override
//	public User newInstance(Object... args) {
//		return new User();
//	}

    // ʹ�õĹ�����
    /**
     * �����������
     */
    public static String UTILS = Constants.optString(JSONTransferableUtilsConstants._JSON_TUTILS);
    /**
     * ��ȡidxMap, filterIdxMap��idxManager
     */
    public static String IDX_MAP_MANAGER = Constants.optString(JSONTransferableUtilsConstants._JSON_TIDX_MAP_MANAGER);
    /**
     * id������
     */
    public static String ID = Constants.optString(JSONTransferableUtilsConstants._JSON_TID);
    /**
     * ѭ����ʱ�����������
     */
    public static String FOREACH_ELEMENT = Constants.optString(JSONTransferableUtilsConstants._JSON_TFOR_EACH_ELE);
    /**
     * BEAN_KEY������
     */
    public static String BEAN_KEY = Constants.optString(JSONTransferableUtilsConstants._JSON_TBEAN_KEY);
    /**
     * PROTO_BEAN������
     */
    public static String PROTO_BEAN_KEY = Constants.optString(JSONTransferableUtilsConstants._JSON_TPROTO_BEAN_KEY);
    /**
     * arrIdxMap �ƺ������� set, add��, ������
     */
    public static String ARR_IDX_MAP_KEY = Constants.optString(JSONTransferableUtilsConstants._JSON_TARR_IDX_MAP_KEY);
    /**
     * Ĭ�ϵ�loadIdx
     */
    public static String DEFAULT_LOAD_IDX = Constants.optString(JSONTransferableUtilsConstants._JSON_TDEFAULT_LOAD_IDX);
    /**
     * Ĭ�ϵ�filterIdx
     */
    public static String DEFAULT_FILTER_IDX = Constants.optString(JSONTransferableUtilsConstants._JSON_TDEFAULT_FILTER_IDX);
    /**
     * ������صĺ�׺
     */
    public static String IDX_SUFFIX = Constants.optString(JSONTransferableUtilsConstants._JSON_TIDX_SUFFIX);
    /**
     * Object��ص���ʱ����ĺ�׺
     */
    public static String OBJ_SUFFIX = Constants.optString(JSONTransferableUtilsConstants._JSON_TOBJ_SUFFIX);
    /**
     * Array��ص���ʱ����ĺ�׺
     */
    public static String ARR_SUFFIX = Constants.optString(JSONTransferableUtilsConstants._JSON_TARR_SUFFIX);

    // �����õķ���
//	public static String toStringDeclare = "	return encapJSON(new JSONObject().element(BEAN_KEY(), defaultLoadIdx() ), new JSONObject().element(BEAN_KEY(), DEFAULT_FILTER_IDX()) ).toString();";

    /**
     * �ӿ� -> һ��Ĭ�ϵ�ʵ����
     */
    public static Map<Class, String> INTER_2_IMPL = Tools.asMap(
            new Class[]{
                    List.class, Set.class, Queue.class
            }, "ArrayList", "HashSet", "LinkedList"
    );
    /**
     * ���Թ̶�Ԫ��������class
     */
    public static Set<String> COULD_FIX_SIZE = Tools.asSet("ArrayList");
    /**
     * Ĭ�ϵ�ClassName
     */
    public static String UNDEFINED_CLAZZ = "UndefinedClazz";

    /**
     * ���ݸ��������� �õ��Ӹ�����JSONObject�л�ȡ��ָ��
     */
    public static JSONObject TYPE_2_COMMAND = new JSONObject()
            .element("int", "getInt").element("long", "getLong").element("boolean", "getBoolean").element("double", "getDouble")
            .element("Integer", "getInt").element("Long", "getLong").element("Boolean", "getBoolean").element("Double", "getDouble")
            .element("String", "getString").element("JSONObject", "getJSONObject").element("JSONArray", "getJSONArray");

    /**
     * ����JSONTransferable����ط���
     *
     * @param utils       ����������
     * @param clazz       ��������Ҫ���ɵ����class
     * @param elemPerLine loadFromJSON, encapJSON ÿһ�ж��ٸ�element
     * @param prefix      ǰ׺, ����еĻ�, �����������״�ǰ׺��idx
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:04 PM
     * @since 1.0
     */
    public static String generateIdxes(String utils, Class clazz, int elemPerLine, String prefix) throws Exception {
        StringBuilder sb = new StringBuilder();
        Field[] fields = clazz.getDeclaredFields();
        fields = collectMemberFields(fields);

        // @Override toString()
        Tools.appendCRLF(sb, "// for debug");
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public String toString() {");
        toString(sb);
        Tools.appendCRLF(sb, "}");

        // loadFromObject�������
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "// loadFromObject�������");
        loadFromJsonIdxes(sb, utils, prefix, clazz, fields);

        // encapJSON���filter
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "// encapJSON���filter");
        encapJsonIdxes(sb, utils, clazz, fields);

        // BEAN_KEY, protoBean
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "public static final String " + getBeanIdxKey(clazz) + " = \"" + Tools.lowerCaseFirstChar(clazz.getSimpleName()) + "_key\";");
        Tools.appendCRLF(sb, "public static final " + clazz.getSimpleName() + " " + getProtoBeanKey(clazz) + " = new " + clazz.getSimpleName() + "();");

        // @Override public BeanType loadFromJSON(JSONObject obj, Map<String, Integer> idxMap)
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public " + clazz.getSimpleName() + " loadFromJSON(Map<String, Object> obj, Map<String, Integer> idxMap) {");
        loadFromJson(sb, utils, clazz, fields);
        Tools.appendCRLF(sb, "}");

        // @Override public BeanType loadFromJSON(JSONObject obj, Map<String, Integer> idxMap)
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public " + clazz.getSimpleName() + " loadFromJSON(Map<String, Object> obj, Map<String, Integer> idxMap, Set<String> initObjFilter) {");
        loadFromJsonWithInitObj(sb, utils, clazz, fields);
        Tools.appendCRLF(sb, "}");

        // @Override public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap)
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap) {");
        encapJson(sb, utils, elemPerLine, clazz, fields);
        Tools.appendCRLF(sb, "}");
        // @Override public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap, Set<Object> cycleDectector)
//		Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap, Deque<Object> cycleDectector) {");
        encapJsonWithDectector(sb, utils, elemPerLine, clazz, fields);
        Tools.appendCRLF(sb, "}");

        // @Override public BeanType newInstance()
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public " + clazz.getSimpleName() + " newInstance(Object... args) {");
        newInstance(sb, clazz);
        Tools.appendCRLF(sb, "}");

        // @Override public String id()
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public String id() {");
        id(sb, clazz);
        Tools.appendCRLF(sb, "}");

        // @Override public String id(String id)
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public void id(String id) {");
        idSetter(sb, clazz);
        Tools.appendCRLF(sb, "}");

        // @Override public String beanKey()
//		Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public String beanKey() {");
        beanKey(sb, clazz);
        Tools.appendCRLF(sb, "}");

        // @Override public BeanType protoBean()
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public " + clazz.getSimpleName() + " protoBean() {");
        protoBean(sb, clazz);
        Tools.appendCRLF(sb, "}");

        // @Override public IdxType defaultLoadIdx()
//		Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public Integer defaultLoadIdx() {");
        defaultLoadIdx(sb, clazz);
        Tools.appendCRLF(sb, "}");

        // @Override public IdxType DEFAULT_FILTER_IDX()
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public Integer defaultFilterIdx() {");
        defaultFilterIdx(sb, clazz);
        Tools.appendCRLF(sb, "}");

        return sb.toString();
    }

    public static String generateIdxes(Class clazz, int elemPerLine, String prefix) throws Exception {
        return generateIdxes(UTILS, clazz, elemPerLine, prefix);
    }

    public static String generateIdxes(Class clazz, int elemPerLine) throws Exception {
        return generateIdxes(UTILS, clazz, elemPerLine);
    }

    public static String generateIdxes(String utils, Class clazz, int elemPerLine) throws Exception {
        return generateIdxes(utils, clazz, elemPerLine, Tools.EMPTY_STR);
    }

//	// AdvInfoDao
//	static interface MysqlAdvInfoDao extends MysqlIBaseDao<AdvInfo, Integer> {
//		
//	}
//	// MysqlAdvInfoDaoImpl
//	static class MysqlAdvInfoDaoImpl extends MysqlBaseDaoImpl<AdvInfo, Integer> implements MysqlAdvInfoDao {
//
//		public MysqlAdvInfoDaoImpl(AdvInfo bean) {
//			super(bean, (MysqlDbConfig) new MysqlDbConfig().ID("_id") );
//		}
//		public MysqlAdvInfoDaoImpl(AdvInfo bean, MysqlDbConfig config) {
//			super(bean, config);
//		}
//		
//	}


    /**
     * ����dao������ - mysql
     */
    public static final String TYPE_MYSQL = "Mysql";
    /**
     * ����dao������ - mongo
     */
    public static final String TYPE_MONGO = "Mongo";
    /**
     * ����dao, daoImpl��ע������
     */
    public static String preDaoDaoImpl = "// ----------------------- dao & daoImpl----------------------------";
    /**
     * type -> ConnectionType
     */
    public static final Map<String, Object> type2ConnectionType = new JSONObject().element(TYPE_MYSQL, "Connection").element(TYPE_MONGO, "MongoClient");


    /**
     * ��ȡ��򵥵�XXXDao, XXXDaoImpl
     *
     * @param clazz ������class
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:07 PM
     * @since 1.0
     */
    public static String generateAllDaoDaoImpl(Class clazz) {
        StringBuilder sb = new StringBuilder();
        Tools.appendCRLF(sb, preDaoDaoImpl);
        for (Entry<String, Object> entry : type2ConnectionType.entrySet()) {
            Tools.appendCRLF(sb, generateDaoDaoImpl(clazz, entry.getKey()));
        }

        return sb.toString();
    }

    /**
     * ���ݸ�����class, ���ɸ��������͵�daoʵ��
     *
     * @param clazz ������class
     * @param type  ��Ҫ���ɵ�dao������
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:08 PM
     * @since 1.0
     */
    public static String generateDaoDaoImpl(Class clazz, String type) {
        if (!type2ConnectionType.containsKey(type)) {
            String debugInfo = "unKnown type : '" + type + "'";
            Log.err(debugInfo);
            return debugInfo;
        }

        StringBuilder sb = new StringBuilder();

        String clazzName = clazz.getSimpleName();
        String daoName = (type + clazzName) + "Dao";
        String daoImplName = (type + clazzName) + "DaoImpl";
        Tools.appendCRLF(sb, "// " + daoName);
        Tools.appendCRLF(sb, "public static interface " + daoName + " extends " + type + "IBaseDao<" + clazzName + ", Integer> {");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "}");
        Tools.appendCRLF(sb, "// " + daoImplName);
        Tools.appendCRLF(sb, "public static class " + daoImplName + " extends " + type + "BaseDaoImpl<" + clazzName + ", Integer> implements " + daoName + " {");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean) {");
        Tools.appendCRLF(sb, "		super(bean);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean, DbConfig config) {");
        Tools.appendCRLF(sb, "		super(bean, config);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean, ConnectionProvider<" + type2ConnectionType.get(type) + "> connectionProvider) {");
        Tools.appendCRLF(sb, "		super(bean, connectionProvider);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean, DbConfig config, ConnectionProvider<" + type2ConnectionType.get(type) + "> connectionProvider) {");
        Tools.appendCRLF(sb, "		super(bean, config, connectionProvider);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "}");

        return sb.toString();
    }

    // --------------- �������� --------------------

    /**
     * toString ���������
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void toString(StringBuilder sb) {
//        Tools.appendCRLF(sb, "	return String.valueOf(encapJSON(" + IDX_MAP_MANAGER + ".doLoadNormalNothingIdxMap, " + IDX_MAP_MANAGER + ".doFilterNothingFilterMap) );");
//		Tools.appendCRLF(sb, "	return encapJSON(new JSONObject().element(BEAN_KEY(), defaultLoadIdx() ), new JSONObject().element(BEAN_KEY(), DEFAULT_FILTER_IDX()) ).toString();" );
//		Tools.appendCRLF(sb, toStringDeclare);
        Tools.appendCRLF(sb, "	return JSONObject.fromObject(this).toString();");
    }

    /**
     * loadFromJson�������
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void loadFromJsonIdxes(StringBuilder sb, String utils, String prefix, Class clazz, Field[] fields) {
        Tools.appendCRLF(sb, "public static final int CAMEL = 0;");
        Tools.appendCRLF(sb, "public static final int UNDER_LINE = CAMEL + 1;");
        boolean addPrefixIdx = (!Tools.isEmpty(prefix));
        if (addPrefixIdx) {
            Tools.appendCRLF(sb, "public static final int PREFIX_CAMEL = UNDER_LINE + 1;");
            Tools.appendCRLF(sb, "public static final int PREFIX_UNDER_LINE = PREFIX_CAMEL + 1;");
        }
        // to be continued ~
        // ..

        for (Field field : fields) {
            String fieldName = field.getName();
            String underLine = Tools.camel2UnderLine(fieldName);
            Tools.append(sb, "public static final String[] " + getIdxName(fieldName) + " = {\"" + fieldName + "\"");
            Tools.append(sb, ", \"" + underLine + "\"");
            if (addPrefixIdx) {
                Tools.append(sb, ", \"" + (prefix + fieldName) + "\"");
                Tools.append(sb, ", \"" + (prefix + underLine) + "\"");
            }
            Tools.appendCRLF(sb, " }; ");
        }
    }

    /**
     * encapJson�������
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void encapJsonIdxes(StringBuilder sb, String utils, Class clazz, Field[] fields) {
        Tools.appendCRLF(sb, "public static final int ALL = 0;");
        Tools.appendCRLF(sb, "public static final int FILTER_ID = ALL + 1;");
        Tools.append(sb, "public static final List<Set<String>> filters = " + utils + ".asList(" + utils + ".asSet(\"\")");
        Tools.append(sb, ", " + utils + ".asSet(" + getIdxName(ID) + ")");
        // to be continued ~
        // ..
        Tools.appendCRLF(sb, ");");
    }

    /**
     * loadFromJson
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void loadFromJson(StringBuilder sb, String utils, Class clazz, Field[] fields) throws Exception {
        Tools.appendCRLF(sb, "	return loadFromJSON(obj, idxMap, Constants.EMPTY_INIT_OBJ_FILTER );");
    }

    /**
     * loadFromJson
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void loadFromJsonWithInitObj(StringBuilder sb, String utils, Class clazz, Field[] fields) throws Exception {
        Tools.appendCRLF(sb, "	if(" + utils + ".isEmpty(obj) || " + utils + ".isEmpty(idxMap) || (idxMap.get(" + getBeanIdxKey(clazz) + ") == null) ) {");
        Tools.appendCRLF(sb, "		return this;");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	int idx = idxMap.get(" + getBeanIdxKey(clazz) + ").intValue();");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        for (Field field : fields) {
            // for 'Collection'
            if (ReflectUtils.implements0(field.getType(), Collection.class)) {
                // for 'Array' [notice : just support 'oneEncapArray']
            } else if (field.getType().isArray()) {
                // for 'Map'
            } else if (ReflectUtils.implements0(field.getType(), Map.class)) {
                Tools.appendCRLF(sb, "	this." + field.getName() + " = " + utils + ".optJSONObject(obj, idx, " + getIdxName(field.getName()) + ");");
                // for 'Other'
                // update for 'initObj' at 2016.06.20
            } else {
//				Tools.appendCRLF(sb, "	this." + field.getName() + " = " + getInitDecalre(UTILS, field));
                String simpleClassName = field.getType().getSimpleName();
                if (TYPE_2_COMMAND.containsKey(simpleClassName)) {
                    Tools.appendCRLF(sb, "	this." + field.getName() + " = " + utils + "." + TYPE_2_COMMAND.get(simpleClassName) + "(obj, idx, " + getIdxName(field.getName()) + ");");
                } else {
                    Tools.appendCRLF(sb, "	if(! initObjFilter.contains(\"" + field.getName() + "\") ) {");
                    Tools.appendCRLF(sb, "		this." + field.getName() + " = " + "(this." + field.getName() + " == null)"
                            + " ? new " + field.getType().getSimpleName() + "().loadFromJSON(" + utils + ".getJSONObject(obj, idx, " + getIdxName(field.getName()) + "), idxMap)"
                            + " : this." + field.getName() + ".loadFromJSON(" + utils + ".getJSONObject(obj, idx, " + getIdxName(field.getName()) + "), idxMap);");
                    Tools.appendCRLF(sb, "	}");
                }
            }
        }

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        for (Field field : fields) {
            // for 'Collection'
            if (ReflectUtils.implements0(field.getType(), Collection.class)) {
                // dummy
                String implClazz = UNDEFINED_CLAZZ;
                for (Map.Entry<Class, String> entry : INTER_2_IMPL.entrySet()) {
                    if (ReflectUtils.implements0(field.getType(), entry.getKey())) {
                        implClazz = entry.getValue();
                    }
                }

                Tools.appendCRLF(sb, "	JSONArray " + getTmpArr(field.getName()) + " = " + utils + ".optJSONArray(obj, idx, " + getIdxName(field.getName()) + ");");
                if (COULD_FIX_SIZE.contains(implClazz)) {
                    Tools.appendCRLF(sb, "	this." + field.getName() + " = new " + implClazz + "(0);");
                } else {
                    Tools.appendCRLF(sb, "	this." + field.getName() + " = new " + implClazz + "();");
                }
                Tools.appendCRLF(sb, "	if(! " + utils + ".isEmpty(" + getTmpArr(field.getName()) + ") ) {");
                if (COULD_FIX_SIZE.contains(implClazz)) {
                    Tools.appendCRLF(sb, "		this." + field.getName() + " = new " + implClazz + "(" + getTmpArr(field.getName()) + ".size() );");
                }

                Tools.appendCRLF(sb, "		if(! initObjFilter.contains(\"" + field.getName() + "\") ) {");
                String genericType = getGenericType(clazz, field.getName());
                Tools.appendCRLF(sb, "			for(int i=0; i<" + getTmpArr(field.getName()) + ".size(); i++) {");
                if (TYPE_2_COMMAND.containsKey(genericType)) {
                    Tools.appendCRLF(sb, "				this." + field.getName() + ".add(" + utils + "." + TYPE_2_COMMAND.get(genericType) + "(" + getTmpArr(field.getName()) + ", i) );");
                } else {
                    Tools.appendCRLF(sb, "				this." + field.getName() + ".add(new " + getGenericType(clazz, field.getName()) + "().loadFromJSON(" + utils + ".getJSONObject(" + getTmpArr(field.getName()) + ", i), idxMap) );");
                }
                Tools.appendCRLF(sb, "			}");
                Tools.appendCRLF(sb, "		}");
                Tools.appendCRLF(sb, "	}");
                Tools.appendCRLF(sb, Tools.EMPTY_STR);

                // for 'Array' [notice : just support 'oneEncapArray']
            } else if (field.getType().isArray()) {
                Tools.appendCRLF(sb, "	JSONArray " + getTmpArr(field.getName()) + " = " + utils + ".optJSONArray(obj, idx, " + getIdxName(field.getName()) + ");");
                Tools.appendCRLF(sb, "	this." + field.getName() + " = new " + getArrayType(clazz, field.getName()) + "[0];");

                Tools.appendCRLF(sb, "	if(! " + utils + ".isEmpty(" + getTmpArr(field.getName()) + ") ) {");
                Tools.appendCRLF(sb, "		this." + field.getName() + " = new " + getArrayType(clazz, field.getName()) + "[" + getTmpArr(field.getName()) + ".size() ];");

                Tools.appendCRLF(sb, "		if(! initObjFilter.contains(\"" + field.getName() + "\") ) {");
                String genericType = getArrayType(clazz, field.getName());
                Tools.appendCRLF(sb, "			for(int i=0; i<" + getTmpArr(field.getName()) + ".size(); i++) {");
                if (TYPE_2_COMMAND.containsKey(genericType)) {
                    Tools.appendCRLF(sb, "				this." + field.getName() + "[i] = " + utils + "." + TYPE_2_COMMAND.get(genericType) + "(" + getTmpArr(field.getName()) + ", i);");
                } else {
                    Tools.appendCRLF(sb, "				this." + field.getName() + "[i] = new " + getArrayType(clazz, field.getName()) + "().loadFromJSON(" + utils + ".getJSONObject(" + getTmpArr(field.getName()) + ", i), idxMap);");
                }
                Tools.appendCRLF(sb, "			}");
                Tools.appendCRLF(sb, "			" + ARR_IDX_MAP_KEY + ".put(\"" + field.getName() + "\", " + getTmpArr(field.getName()) + ".size() );");
                Tools.appendCRLF(sb, "		} else {");
                Tools.appendCRLF(sb, "			" + ARR_IDX_MAP_KEY + ".put(\"" + field.getName() + "\", 0);");
                Tools.appendCRLF(sb, "		}");
                Tools.appendCRLF(sb, "	}");
                Tools.appendCRLF(sb, Tools.EMPTY_STR);

                // for 'Map'
            } else if (ReflectUtils.implements0(field.getType(), Map.class)) {

                // for 'Other'
            } else {

            }
        }
        Tools.appendCRLF(sb, "	return this;");
    }

    /**
     * encapJson
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void encapJson(StringBuilder sb, String utils, int elemPerLine, Class clazz, Field[] fields) throws Exception {
        Tools.appendCRLF(sb, "	return encapJSON(idxMap, filterIdxMap, new LinkedList<Object>() );");
    }

    /**
     * encapJson
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void encapJsonWithDectector(StringBuilder sb, String utils, int elemPerLine, Class clazz, Field[] fields) throws Exception {
        Tools.appendCRLF(sb, "	if(cycleDectector.contains(this) ) {");
        Tools.appendCRLF(sb, "		return JSONObject.fromObject(Constants.OBJECT_ALREADY_EXISTS).element(\"id\", String.valueOf(id()) );");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	cycleDectector.push(this);");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "	if(" + utils + ".isEmpty(idxMap) || (idxMap.get(" + getBeanIdxKey(clazz) + ") == null) ) {");
        Tools.appendCRLF(sb, "		cycleDectector.pop();");
        Tools.appendCRLF(sb, "		return null;");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	int idx = idxMap.get(" + getBeanIdxKey(clazz) + ").intValue();");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        for (int i = 0; i < fields.length; i++) {
            Field field = fields[i];
            // for 'Collection'
            if (ReflectUtils.implements0(field.getType(), Collection.class)) {
                // dummy
                Tools.appendCRLF(sb, "	JSONArray " + getTmpArr(field.getName()) + " = new JSONArray();");
                Tools.appendCRLF(sb, "	if(! " + utils + ".isEmpty(this." + field.getName() + ") ) {");

                String genericType = getGenericType(clazz, field.getName());
                Tools.appendCRLF(sb, "		for(" + genericType + " " + FOREACH_ELEMENT + " : this." + field.getName() + ") {");
//				if(TYPE_2_COMMAND.containsKey(genericType) ) {
//					Tools.appendCRLF(sb, "			" + getTmpArr(field.getName()) + ".add(ele);" );
//				} else {
//					Tools.appendCRLF(sb, "			" + getTmpArr(field.getName()) + ".add(ele.encapJSON(idxMap, filterIdxMap) );" );
//				}
                Tools.appendCRLF(sb, "			" + getTmpArr(field.getName()) + ".add(" + getToStringDecalre(genericType, FOREACH_ELEMENT) + ");");

                Tools.appendCRLF(sb, "		}");
                Tools.appendCRLF(sb, "	}");
                Tools.appendCRLF(sb, Tools.EMPTY_STR);

                // for 'Array' [notice : just support 'oneEncapArray']
            } else if (field.getType().isArray()) {
                Tools.appendCRLF(sb, "	JSONArray " + getTmpArr(field.getName()) + " = new JSONArray();");
                Tools.appendCRLF(sb, "	if(! " + utils + ".isEmpty(this." + field.getName() + ") ) {");

                String genericType = getArrayType(clazz, field.getName());
                Tools.appendCRLF(sb, "		for(" + genericType + " " + FOREACH_ELEMENT + " : this." + field.getName() + ") {");
                Tools.appendCRLF(sb, "			" + getTmpArr(field.getName()) + ".add(" + getToStringDecalre(genericType, FOREACH_ELEMENT) + ");");

                Tools.appendCRLF(sb, "		}");
                Tools.appendCRLF(sb, "	}");
                Tools.appendCRLF(sb, Tools.EMPTY_STR);

                // for 'Map'	doNothing, just put it
//			} else if(ReflectTools.implements0(field.getType(), Map.class) ) {
//				
//			// for 'Other'
//			} else {
//				Tools.append(sb, ".element(" + getIdxName(field.getName() ) + "[" + UTILS + ".getIdx(idx, " + getIdxName(field.getName() ) + ")], " + getToStringDecalre(field) + ")");
            }
        }

        Tools.append(sb, "	JSONObject res = new JSONObject()");
        for (int i = 0; i < fields.length; i++) {
            if ((i % elemPerLine) == 0) {
                Tools.appendCRLF(sb, Tools.EMPTY_STR);
                Tools.append(sb, "		");
            }

            Field field = fields[i];
            // for 'Collection'
            if (ReflectUtils.implements0(field.getType(), Collection.class)) {
                Tools.append(sb, ".element(" + getIdxName(field.getName()) + "[" + utils + ".getIdx(idx, " + getIdxName(field.getName()) + ")], " + getTmpArr(field.getName()) + ")");
                // for 'Array' [notice : just support 'oneEncapArray']
            } else if (field.getType().isArray()) {
                Tools.append(sb, ".element(" + getIdxName(field.getName()) + "[" + utils + ".getIdx(idx, " + getIdxName(field.getName()) + ")], " + getTmpArr(field.getName()) + ")");
                // for 'Map'
            } else if (ReflectUtils.implements0(field.getType(), Map.class)) {
                Tools.append(sb, ".element(" + getIdxName(field.getName()) + "[" + utils + ".getIdx(idx, " + getIdxName(field.getName()) + ")], " + field.getName() + ")");
                // for 'Other'
            } else {
                Tools.append(sb, ".element(" + getIdxName(field.getName()) + "[" + utils + ".getIdx(idx, " + getIdxName(field.getName()) + ")], " + getToStringDecalre(field) + ")");
            }
        }
        Tools.appendCRLF(sb, ";");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "	if(" + utils + ".isEmpty(filterIdxMap) || (filterIdxMap.get(" + getBeanIdxKey(clazz) + ") == null) ) {");
        Tools.appendCRLF(sb, "		cycleDectector.pop();");
        Tools.appendCRLF(sb, "		return res;");
        Tools.appendCRLF(sb, "	}");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "	cycleDectector.pop();");
        Tools.appendCRLF(sb, "	int filterIdx = filterIdxMap.get(" + getBeanIdxKey(clazz) + ").intValue();");
        Tools.appendCRLF(sb, "	return " + utils + ".filter(res, filters.get(" + utils + ".getIdx(filterIdx, filters.size())) );");
    }

    /**
     * newInstance
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void newInstance(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return new " + clazz.getSimpleName() + "();");
    }

    /**
     * id
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void id(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return " + ID + ";");
    }

    /**
     * id
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void idSetter(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	this." + ID + " = id;");
    }

    /**
     * beanKey
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void beanKey(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return " + getBeanIdxKey(clazz) + ";");
    }

    /**
     * protoBean
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void protoBean(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return " + getProtoBeanKey(clazz) + ";");
    }

    /**
     * defaultLoadIdx
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void defaultLoadIdx(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return " + DEFAULT_LOAD_IDX + ";");
    }

    /**
     * defaultFilterIdx
     *
     * @param sb ��������Ҫ�����sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void defaultFilterIdx(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return " + DEFAULT_FILTER_IDX + ";");
    }

    /**
     * ��ȡ����������
     *
     * @param fieldName �������ֶε�����
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static String getIdxName(String fieldName) {
        return fieldName + IDX_SUFFIX;
    }

    /**
     * ��ȡ�����Ķ����ֶζ�Ӧ����ʱ���������
     *
     * @param fieldName �������ֶε�����
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static String getTmpObj(String fieldName) {
        return fieldName + OBJ_SUFFIX;
    }

    /**
     * ��ȡ�����������ֶζ�Ӧ����ʱ���������
     *
     * @param fieldName �������ֶε�����
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static String getTmpArr(String fieldName) {
        return fieldName + ARR_SUFFIX;
    }

    /**
     * ��ȡ�������ֶεĵ�һ�����Ͳ���
     *
     * @param fieldName �������ֶε�����
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static String getGenericType(Class clazz, String fieldName) throws Exception {
        return getGenericType(clazz.getDeclaredField(fieldName));
    }

    /**
     * ��ȡ�������ֶεĵ�һ�����Ͳ���
     *
     * @param field �������ֶ�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:12 PM
     * @since 1.0
     */
    private static String getGenericType(Field field) throws Exception {
        ParameterizedType genericType = (ParameterizedType) field.getGenericType();
        Type[] actualTypeArguements = genericType.getActualTypeArguments();

        if ((actualTypeArguements == null) || (actualTypeArguements.length == 0)) {
            return null;
        }

        if (actualTypeArguements[0] instanceof Class) {
            return ((Class) actualTypeArguements[0]).getSimpleName();
        }
        // incase of 'CompositeTypes'
        return UNDEFINED_CLAZZ;
    }

    /**
     * ��ȡ�����������ֶε�Ԫ������
     *
     * @param clazz     ������class
     * @param fieldName �������ֶ�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:12 PM
     * @since 1.0
     */
    private static String getArrayType(Class clazz, String fieldName) throws Exception {
        return getArrayType(clazz.getDeclaredField(fieldName));
    }

    /**
     * ��ȡ�����������Ԫ������
     *
     * @param field �������ֶ�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:12 PM
     * @since 1.0
     */
    private static String getArrayType(Field field) throws Exception {
        Class componentClazz = field.getType().getComponentType();
        // incase of 'CompositeTypes'
        if (ReflectUtils.implements0(componentClazz, Collection.class) || (componentClazz.isArray())) {
            return UNDEFINED_CLAZZ;
        }

        return componentClazz.getSimpleName();
    }

    /**
     * ��ȡ������class��BeanKey
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:12 PM
     * @since 1.0
     */
    private static String getBeanIdxKey(Class clazz) {
        return BEAN_KEY;
    }

    /**
     * ��ȡ������class��PROTO_BEAN_KEY
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:12 PM
     * @since 1.0
     */
    private static String getProtoBeanKey(Class clazz) {
        return PROTO_BEAN_KEY;
    }

    /**
     * ��ȡ��ʼ���������ֶε����, �򵥵�����, optXX, ���ص�����loadFromJSON
     *
     * @param utils ������
     * @param field �������ֶ�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:14 PM
     * @since 1.0
     */
    private static String getInitDecalre(String utils, Field field) {
        String simpleClassName = field.getType().getSimpleName();
        if (TYPE_2_COMMAND.containsKey(simpleClassName)) {
            return utils + "." + TYPE_2_COMMAND.get(simpleClassName) + "(obj, idx, " + getIdxName(field.getName()) + ");";
        }

        return "(this." + field.getName() + " == null)"
                + " ? new " + field.getType().getSimpleName() + "().loadFromJSON(" + utils + ".getJSONObject(obj, idx, " + getIdxName(field.getName()) + "), idxMap)"
                + " : this." + field.getName() + ".loadFromJSON(" + utils + ".getJSONObject(obj, idx, " + getIdxName(field.getName()) + "), idxMap);";
    }

    /**
     * ��ȡ��ǰfield���ַ�����ʾ
     *
     * @param field �������ֶ�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:15 PM
     * @since 1.0
     */
    private static String getToStringDecalre(Field field) {
        return getToStringDecalre(field.getType().getSimpleName(), field.getName());
    }

    /**
     * ��ȡ�������ֶε��ַ�����ʾ
     *
     * @param type      ����������
     * @param fieldName �������ֶ�����
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:15 PM
     * @since 1.0
     */
    private static String getToStringDecalre(String type, String fieldName) {
        if (TYPE_2_COMMAND.containsKey(type)) {
            return fieldName;
        }
        return "(" + fieldName + " == null) ? \"null\" : " + fieldName + ".encapJSON(idxMap, filterIdxMap, cycleDectector)";

    }

    /**
     * ȥ���������б��еĲ����ڳ�Ա�ֶεĳ�Ա
     *
     * @param fields fields
     * @return java.lang.reflect.Field[]
     * @author Jerry.X.He
     * @date 5/20/2017 11:18 AM
     * @since 1.0
     */
    private static Field[] collectMemberFields(Field[] fields) {
        List<Field> memeberFields = new ArrayList<>(fields.length);
        for (Field field : fields) {
            int mod = field.getModifiers();
            if (!Modifier.isStatic(mod)) {
                memeberFields.add(field);
            }
        }

        Field[] result = new Field[memeberFields.size()];
        memeberFields.toArray(result);
        return result;
    }

}
