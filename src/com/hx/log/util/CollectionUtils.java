/**
 * file name : CollectionUtils.java
 * created at : 22:36:23 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import net.sf.json.JSONObject;

public final class CollectionUtils {
	
	// disable constructor
	private CollectionUtils() {
		Tools.assert0("can't instantiate !");
	}
	
	
	// 判断给定的集合是否为空
	public static <T> boolean isEmpty(Collection<T> arr) {
		return (arr == null) || (arr.size() == 0) || (arr.isEmpty() );
	}
	public static <K, V> boolean isEmpty(Map<K, V> map) {
		return (map == null) || (map.size() == 0) || (map.isEmpty() );
	}
	
	// add at 2016.06.02
	public static <T> boolean isEmpty(T[] arr) {
		return (arr == null) || (arr.length == 0);
	}
	public static boolean isEmpty(int[] arr) {
		return (arr == null) || (arr.length == 0);
	}
	public static boolean isEmpty(long[] arr) {
		return (arr == null) || (arr.length == 0);
	}
	public static boolean isEmpty(boolean[] arr) {
		return (arr == null) || (arr.length == 0);
	}
	public static boolean isEmpty(double[] arr) {
		return (arr == null) || (arr.length == 0);
	}
	
	
	// ------------ asList / Set / Map ------- 2016.04.24 -------------
   public static <T> List<T> asList(T... eles) {
	   return new ArrayList0<>(eles);
   }
   public static <T> List<T> asLinkedList(T... eles) {
	   return new LinkedList0<>(eles);
   }
   public static <T> List<T> asList(T[]... eles) {
	   List<T> res = new ArrayList0<T>();
	   for(T[] ele : eles) {
		   asList(res, ele);
	   }
	   return res;
   }
   public static <T> List<T> asLinkedList(T[]... eles) {
	   List<T> res = new LinkedList0<T>();
	   for(T[] ele : eles) {
		   asList(res, ele);
	   }
	   return res;
   }
   public static <T> List<T> asList(List<T> ls, T... eles) {
	   Tools.assert0(ls != null, "'ls' can't be null ");
	   for(T ele : eles) {
		   ls.add(ele);
	   }
	   return ls;
   }
   public static <T> Set<T> asSet(T... eles) {
	   return new HashSet0<>(eles);
   }
   public static <T> Set<T> asLinkedSet(T... eles) {
	   return new LinkedHashSet0<>(eles);
   }
   public static <T> Set<T> asSortedSet(T... eles) {
	   return new TreeSet0<>(eles);
   }
   public static <T> Set<T> asSet(T[]... eles) {
	   Set<T> res = new HashSet0<T>();
	   for(T[] ele : eles) {
		   asSet(res, ele);
	   }
	   return res;
   }
   public static <T> Set<T> asLinkedSet(T[]... eles) {
	   Set<T> res = new LinkedHashSet0<T>();
	   for(T[] ele : eles) {
		   asSet(res, ele);
	   }
	   return res;
   }
   public static <T> Set<T> asSortedSet(T[]... eles) {
	   Set<T> res = new TreeSet0<T>();
	   for(T[] ele : eles) {
		   asSet(res, ele);
	   }
	   return res;
   }
   public static <T> Set<T> asSet(Set<T> set, T... eles) {
	   Tools.assert0(set != null, "'set' can't be null ");
	   for(T ele : eles) {
		   set.add(ele);
	   }
	   return set;
   }
   public static <K, V> Map<K, V> asMap(K key, V val) {
	   return new HashMap0<>(key, val);
   }
   public static <K, V> Map<K, V> asLinkedMap(K key, V val) {
	   return new LinkedHashMap0<>(key, val);
   }
   public static <K, V> Map<K, V> asSortedMap(K key, V val) {
	   return new TreeMap0<>(key, val);
   }
   public static <K, V> Map<K, V> asMap(K[] keys, V... vals) {
	   return new HashMap0<>(keys, vals);
   }
   public static <K, V> Map<K, V> asLinkedMap(K[] keys, V... vals) {
	   return new LinkedHashMap0<>(keys, vals);
   }
   public static <K, V> Map<K, V> asSortedMap(K[] keys, V... vals) {
	   return new TreeMap0<>(keys, vals);
   }
   public static <K, V> Map<K, V> asMap(Map<K, V> map, K[] keys, V... vals) {
	   Tools.assert0(map != null, "'map' can't be null ");
	   for(int i=0; i<keys.length; i++) {
		   map.put(keys[i], vals[i]);
	   }
	   return map;
   }
   
   // 辅助数据结构
   private static class ArrayList0<E> extends ArrayList<E> {
	   public ArrayList0(E... array) {
           if (array == null)	return ;
            for(E ele : array) {
            	add(ele);
            }
	   }
   }
	private static class LinkedList0<E> extends LinkedList<E> {
	   public LinkedList0(E... array) {
            if (array == null)	return ;
            for(E ele : array) {
            	add(ele);
            }
	   }
   }
	private  static class HashSet0<E> extends HashSet<E> {
	   public HashSet0(E... array) {
           if (array == null)	return ;
            for(E ele : array) {
            	add(ele);
            }
	   }
   }
	private   static class LinkedHashSet0<E> extends LinkedHashSet<E> {
	   public LinkedHashSet0(E... array) {
		   if (array == null)	return ;
		   for(E ele : array) {
			   add(ele);
		   }
	   }
   }
	private  static class TreeSet0<E> extends TreeSet<E> {
	   public TreeSet0(E... array) {
           if (array == null)	return ;
            for(E ele : array) {
            	add(ele);
            }
	   }
   }
	private  static class HashMap0<K, V> extends HashMap<K, V> {
	   public HashMap0(K key, V val) {
		   if ((key == null) )	return ;
		   put(key, val);
	   }
	   public HashMap0(K[] keys, V... vals) {
            if ((keys == null) || (vals == null) )	return ;
            Tools.assert0(keys.length == vals.length, "keys's length must 'eq' vals's length !");
            for(int i=0; i<keys.length; i++) {
            	put(keys[i], vals[i]);
            }
	   }
   }
	private  static class LinkedHashMap0<K, V> extends LinkedHashMap<K, V> {
	   public LinkedHashMap0(K key, V val) {
		   if ((key == null) )	return ;
		   put(key, val);
	   }
	   public LinkedHashMap0(K[] keys, V... vals) {
		   if ((keys == null) || (vals == null) )	return ;
		   Tools.assert0(keys.length == vals.length, "keys's length must 'eq' vals's length !");
		   for(int i=0; i<keys.length; i++) {
			   put(keys[i], vals[i]);
		   }
	   }
   }
	private  static class TreeMap0<K, V> extends TreeMap<K, V> {
	   public TreeMap0(K key, V val) {
		   if ((key == null) )	return ;
		   put(key, val);
	   }
	   public TreeMap0(K[] keys, V... vals) {
           if ((keys == null) || (vals == null) )	return ;
           Tools.assert0(keys.length == vals.length, "keys's length must 'eq' vals's length !");
           for(int i=0; i<keys.length; i++) {
        	   put(keys[i], vals[i]);
           }
	   }
   }

   
	// add at 2016.08.11
	// 判断给定的字符数组中是否包含给定的字符
	public static boolean contains(int[] arr, int ele) {
		if(arr == null) {
			return false;
		}
		
		for(int i=0; i<arr.length; i++) {
			if(arr[i] == ele) {
				return true;
			}
		}
		
		return false;
	}
	public static boolean contains(long[] arr, long ele) {
		if(arr == null) {
			return false;
		}
		
		for(int i=0; i<arr.length; i++) {
			if(arr[i] == ele) {
				return true;
			}
		}
		
		return false;
	}
	public static boolean contains(double[] arr, double ele) {
		if(arr == null) {
			return false;
		}
		
		for(int i=0; i<arr.length; i++) {
			if(GeometryUtils.equals(arr[i], ele) ) {
				return true;
			}
		}
		
		return false;
	}
	public static boolean contains(char[] arr, char ele) {
		if(arr == null) {
			return false;
		}
		
		for(int i=0; i<arr.length; i++) {
			if(arr[i] == ele) {
				return true;
			}
		}
		
		return false;
	}
	public static boolean contains(boolean[] arr, boolean ele) {
		if(arr == null) {
			return false;
		}
		
		for(int i=0; i<arr.length; i++) {
			if(arr[i] == ele) {
				return true;
			}
		}
		
		return false;
	}
	public static <T> boolean contains(T[] arr, T ele) {
		if(arr == null) {
			return false;
		}
		
		for(int i=0; i<arr.length; i++) {
			if(ele.equals(arr[i]) ) {
				return true;
			}
		}
		
		return false;
	}   
   

}
